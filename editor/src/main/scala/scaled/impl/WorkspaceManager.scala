//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.google.common.collect.HashBiMap
import com.google.common.collect.HashMultimap
import java.io.IOException
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import java.util.{List => JList}
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.Stage
import scala.io.Source
import scaled._
import scaled.pacman.Filez
import scaled.util.Errors

/** Manages workspaces, and the creation of editors therein. */
class WorkspaceManager (app :Scaled) extends AbstractService with WorkspaceService {

  private def log = app.logger
  // the directory in which we store workspace metadata
  private val wsdir = Files.createDirectories(app.pkgMgr.metaDir.resolve("Workspaces"))

  // a map from workspace name to hint path list
  private val wshints = Filer.fileDB[HashMultimap[String,String]](
    wsdir.resolve(".hintpaths"),
    _.fold(HashMultimap.create[String,String]()) { (m, s) =>
      val (w :: ps) = List.from(s.split("\t")) ; ps foreach { m.put(w, _) } ; m
    },
    _.asMap.entrySet.foldBuild[String]((b, e) => b += e.getKey+"\t"+e.getValue.mkString("\t")))

  // currently resolved workspaces
  private var wscache = Mutable.cacheMap { name :String => {
    val ws = new WorkspaceImpl(app, WorkspaceManager.this, name, wsdir.resolve(name))
    app.workspaceOpened.emit(ws)
    ws
  }}

  /** Visits `paths` in the appropriate workspace windows, creating them as needed.
    * The first window created will inherit the supplied default stage. */
  def visit (stage :Stage, paths :JList[String]) {
    val ws2paths = ((paths map resolve) groupBy workspaceFor).toSeq
    // the first workspace (chosen arbitrarily) gets the default stage
    ws2paths.take(1) foreach { case (ws, paths) =>
      paths foreach { ws.open(stage).visitPath _ }
    }
    // the remaining workspaces (if any) create new stages
    ws2paths.drop(1) foreach { case (ws, paths) =>
      paths foreach { ws.open().visitPath _ }
    }
    // if we have no arguments, open the default workspace with a scratch buffer
    if (ws2paths.isEmpty) defaultWS.open(stage).visitScratch()
  }

  /** Visits `path` in the appropriate workspace window. If no window exists one is created. */
  def visit (path :String) {
    val p = resolve(path)
    workspaceFor(p).open().visitPath(p)
  }

  private def resolve (path :String) :Path = {
    val p = Paths.get(path).normalize
    // attempt to absolute-ize the path; if it does not exist and is not already absolute, fall
    // back to tacking in onto the cwd so that we a chance at properly deducing workspace
    if (p.isAbsolute) p else if (Files.exists(p)) p.toAbsolutePath else cwd.resolve(p)
  }

  def checkExit () {
    // if no workspaces have windows open, it's time to go
    if (wscache.asMap.values.forall(_.windows.isEmpty)) Platform.exit()
  }

  def addHintPath (wsname :String, path :Path) :Unit =
    wshints.update(m => { m.put(wsname, path.toString) ; m })
  def removeHintPath (wsname :String, path :Path) :Unit =
    wshints.update(m => { m.remove(wsname, path.toString) ; m })

  override def list = try {
    Files.list(wsdir).collect(Collectors.toList[Path]).filter(Files.isDirectory(_)).
      map(_.getFileName.toString).filterNot(_ startsWith ".").toSeq
  } catch {
    case e :IOException => log.log("Failed to list $wsdir", e) ; Seq.empty
  }

  override def create (wsname :String) {
    val root = wsdir.resolve(wsname)
    if (Files.exists(root)) throw Errors.feedback(s"Workspace named $wsname already exists.")
    Files.createDirectory(root)
    open(wsname)
  }

  override def open (wsname :String) {
    val root = wsdir.resolve(wsname)
    if (!Files.exists(root)) throw Errors.feedback(s"No workspace named $wsname")
    wscache.get(wsname).open().stageToFront()
  }

  override def didStartup () {} // unused
  override def willShutdown () {} // unused

  private def defaultWS = {
    val ddir = wsdir.resolve(Workspace.DefaultName)
    if (!Files.exists(ddir)) Files.createDirectory(ddir)
    wscache.get(Workspace.DefaultName)
  }

  // returns the workspace that should be used for the specified path (which should be absolute and
  // normalized): if a workspace with matching hint path exists, it is used, otherwise the default
  // workspace is used
  private def workspaceFor (path :Path) :WorkspaceImpl = {
    def isOpen (ws :WorkspaceImpl) = !ws.windows.isEmpty
    def latest (ws :Unordered[WorkspaceImpl]) =
      if (ws.isEmpty) None else Some(ws.maxBy(_.lastOpened))
    val hintWSs = wshints().asMap.toMapV collect {
      case (w, ps) if (ps.exists(path startsWith _)) => wscache.get(w) }
    latest(hintWSs.filter(isOpen)) orElse latest(hintWSs) getOrElse defaultWS
  }
}

class WorkspaceImpl (
  val app :Scaled, val mgr :WorkspaceManager, val name :String, val root :Path
) extends Workspace {

  val windows = SeqBuffer[WindowImpl]()
  val buffers = SeqBuffer[BufferImpl]()

  def lastOpened = Files.getLastModifiedTime(root).toMillis

  def open (stage :Stage) = windows.headOption || createWindow(stage, geomSysProp)
  def open () = windows.headOption || createWindow(new Stage(), NoGeom)

  def close (win :WindowImpl) {
    windows.remove(win) // TODO: event
    try {
      win.dispose() // this may cause us to hibernate
      win.stage.close()
    } catch {
      case e :Throwable => log.log(s"Error closing $win", e)
    }
  }

  /** Records a message to this workspace's `*messages*` buffer. */
  def recordMessage (msg :String) {
    // we may get a recordMessage call while we're creating the *messages* buffer, so to avoid
    // the infinite loop of doom in that case, we buffer messages during that process
    if (_pendingMessages != null) _pendingMessages = msg :: _pendingMessages
    else {
      // create or recreate the *messages* buffer as needed
      val mb = buffers.find(_.name == MessagesName) || newMessages()
      mb.append(Line.fromText(msg + System.lineSeparator))
    }
  }

  def focusedBuffer (buffer :BufferImpl) {
    buffers -= buffer
    buffers prepend buffer
  }

  override def editor = app
  override val config = app.cfgMgr.editorConfig(Config.Scope(state, app.state))

  override def createBuffer (name :String, state :List[State.Init[_]],
                             reuse :Boolean) :BufferImpl = {
    def isScratch (name :String) = (name startsWith "*") && (name endsWith "*")
    def create (name :String) = {
      val parent = buffers.headOption.map(b => Paths.get(b.store.parent)) || cwd
      val buf = if (isScratch(name)) BufferImpl.scratch(name)
                else BufferImpl(Store(parent.resolve(name)))
      state foreach { _.apply(buf.state) }
      addBuffer(buf)
    }
    buffers.find(_.name == name) match {
      case None     => create(name)
      case Some(ob) => if (reuse) ob else create(freshName(name))
    }
  }
  override def openBuffer (store :Store) =
    buffers.find(_.store == store) || addBuffer(BufferImpl(store))
  override def killBuffer (buffer :Buffer) = {
    if (buffers.remove(buffer)) buffer.asInstanceOf[BufferImpl].killed.emit(())
    else log.log(s"Requested to kill unknown buffer [ws=$this, buf=$buffer]")
  }

  override def addHintPath (path :Path) :Unit = mgr.addHintPath(name, path)
  override def removeHintPath (path :Path) :Unit = mgr.removeHintPath(name, path)

  override def toString = s"ws:$name"
  override protected def log = app.logger
  override protected def hibernate () {
    super.hibernate()
    buffers.clear()
    mgr.checkExit()
  }

  private final val ScratchName = "*scratch*"
  def newScratch () = createBuffer(ScratchName, Nil, true) // TODO: call this getScratch and reuse

  private final val MessagesName = "*messages*"
  private def newMessages () = {
    _pendingMessages = Nil
    val mbuf = createBuffer(MessagesName, State.inits(Mode.Hint("log")), true)
    _pendingMessages foreach { msg =>
      mbuf.append(Line.fromText(msg + System.lineSeparator))
    }
    _pendingMessages = null
    mbuf
  }
  private var _pendingMessages :List[String] = null

  private def addBuffer (buf :BufferImpl) :BufferImpl = {
    buffers += buf
    bufferOpened.emit(buf)
    buf
  }

  private def createWindow (stage :Stage, geom :Geom) :WindowImpl = {
    val bufferSize = geom.size getOrElse {
      (config(EditorConfig.viewWidth), config(EditorConfig.viewHeight))
    }
    val win = new WindowImpl(stage, this, bufferSize)
    // TODO: willCreateWindow

    val scene = new Scene(win)
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    val os = System.getProperty("os.name").replaceAll(" ", "").toLowerCase
    val oscss = getClass.getResource(s"/$os.css")
    if (oscss != null) scene.getStylesheets().add(oscss.toExternalForm)
    else app.logger.log(s"Unable to locate /$os.css")
    stage.setScene(scene)

    // set our stage position based on the values specified in editor config
    config.value(EditorConfig.viewLeft) onValueNotify { x =>
      if (x >= 0) stage.setX(x)
    }
    config.value(EditorConfig.viewTop) onValueNotify { y =>
      if (y >= 0) stage.setY(y)
    }
    // if geometry was specified on the command line, override the value from prefs
    geom.pos.foreach { pos => stage.setX(pos._1) ; stage.setY(pos._2) }

    // update our last "opened" time when a new editor is created
    Files.setLastModifiedTime(root, FileTime.fromMillis(System.currentTimeMillis))

    // if we're opening the first window in this workspace, start routing log messages to our
    // *messages* buffer
    if (windows.isEmpty) toClose += app.log.onValue(recordMessage)

    windows += win // TODO: didCreateWindow
    win
  }

  private def freshName (name :String, n :Int = 1) :String = {
    val revName = s"$name<$n>"
    if (buffers.exists(_.name == revName)) freshName(name, n+1)
    else revName
  }

  // TODO: move to Editor.Geom? might use in Workspace.createEditor()
  case class Geom (size :Option[(Int,Int)], pos :Option[(Int,Int)])
  val NoGeom = Geom(None, None)
  def geomSysProp = System.getProperty("geometry") match {
    case null => NoGeom
    case geom => geom.split("[x+]") match {
      case Array(w, h, x, y) => Geom(Some(w.toInt -> h.toInt), Some(x.toInt -> y.toInt))
      case Array(w, h)       => Geom(Some(w.toInt -> h.toInt), None)
      case _                 => NoGeom
    }
  }
}
