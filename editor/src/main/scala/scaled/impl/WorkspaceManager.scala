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
  private var wscache = Mutable.cacheMap { name :String =>
    new WorkspaceImpl(app, WorkspaceManager.this, name, wsdir.resolve(name)) }

  /** Creates the starting workspace and editor and opens `paths` therein. */
  def visit (stage :Stage, paths :JList[String]) {
    val desktop = curDesktop
    val path = if (paths.isEmpty) None else Some(paths.get(0))
    val epane = workspaceFor(desktop, path).open(desktop, stage)
    paths foreach epane.visitPath
  }

  /** Visits `path` in the editor on the desktop detected to be current.
    * If no such editor pane exists one is created. */
  def visit (path :String) {
    val desktop = curDesktop
    workspaceFor(desktop, Some(path)).open(desktop).visitPath(path)
  }

  def checkExit () {
    // if no workspaces have editors open, it's time to go
    if (wscache.asMap.values.forall(_.editors.isEmpty)) Platform.exit()
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
    wscache.get(wsname).open(curDesktop).stageToFront()
  }

  override def didStartup () {} // unused
  override def willShutdown () {} // unused

  private def curDesktop :String = System.getProperty("scaled.curdesk", "") match {
    case ""  => "default"
    case bin => try {
      val pb = new ProcessBuilder(bin)
      pb.redirectErrorStream(true)
      val proc = pb.start()
      val lines = Source.fromInputStream(proc.getInputStream).getLines.toList
      proc.waitFor match {
        case 0 => lines.head
        case n =>
          log.log("Failed to read current desktop ($bin failed $n)")
          lines foreach log.log
          "default"
      }
    } catch {
      case e :Exception =>
        log.log("Failed to read current desktop ($bin failed)", e)
        "default"
    }
  }

  // returns the workspace that should be used for the specified desktop:
  // - if a workspace with matching hint path exists, it is used
  // - otherwise, if a workspace is already open on `desktop`, it is used
  // - otherwise, the default workspace is used
  private def workspaceFor (desktop :String, path :Option[String]) :WorkspaceImpl = {
    def isOpen (ws :WorkspaceImpl) = ws.editors.containsKey(desktop)
    def latest (ws :Unordered[WorkspaceImpl]) =
      if (ws.isEmpty) None else Some(ws.minBy(_.lastOpened))
    def abs (p :Path) = if (Files.exists(p)) Some(p.toAbsolutePath.toString) else None
    def byHintPath = path.map(Paths.get(_)).flatMap(abs).flatMap { path =>
      val hintWSs = wshints().asMap.toMapV collect {
        case (w, ps) if (ps.exists(path startsWith _)) => wscache.get(w) }
      latest(hintWSs.filter(isOpen)) orElse latest(hintWSs)
    }
    def latestOpenWS = latest(wscache.asMap.values.filter(isOpen))
    byHintPath orElse latestOpenWS getOrElse {
      val ddir = wsdir.resolve(Workspace.DefaultName)
      if (!Files.exists(ddir)) Files.createDirectory(ddir)
      wscache.get(Workspace.DefaultName)
    }
  }
}

class WorkspaceImpl (
  val app :Scaled, val mgr :WorkspaceManager, val name :String, val root :Path
) extends Workspace {

  // each workspace maintains its own configuration repository
  val cfgMgr = app.svcMgr.injectInstance(classOf[ConfigManager], root :: Nil)

  val editors = HashBiMap.create[String,EditorPane]()
  def editor (desk :String) = Option(editors.get(desk))

  def lastOpened = Files.getLastModifiedTime(root).toMillis

  def open (desk :String, stage :Stage) = editor(desk) getOrElse create(stage, desk, geomSysProp)
  def open (desk :String) = editor(desk) getOrElse create(new Stage(), desk, NoGeom)

  def close (epane :EditorPane) {
    editors.inverse.remove(epane)
    try {
      epane.dispose()
      epane.stage.close()
    } catch {
      case e :Throwable => log.log(s"Error closing $epane", e)
    }
    // TODO: if editors is empty and we have references, complain?
  }

  override def addHintPath (path :Path) :Unit = mgr.addHintPath(name, path)
  override def removeHintPath (path :Path) :Unit = mgr.removeHintPath(name, path)

  override def toString = s"ws:$name"
  override protected def log = app.logger
  override protected def hibernate () {
    super.hibernate()
    mgr.checkExit()
  }

  private def create (stage :Stage, desk :String, geom :Geom) :EditorPane = {
    val bufferSize = geom.size getOrElse {
      val config = cfgMgr.editorConfig
      (config(EditorConfig.viewWidth), config(EditorConfig.viewHeight))
    }
    val epane = new EditorPane(stage, this, bufferSize)
    // stuff this editor's desktop id into global editor state
    epane.state[Editor.Desktop].update(Editor.Desktop(desk))

    val scene = new Scene(epane)
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    val os = System.getProperty("os.name").replaceAll(" ", "").toLowerCase
    val oscss = getClass.getResource(s"/$os.css")
    if (oscss != null) scene.getStylesheets().add(oscss.toExternalForm)
    else app.logger.log(s"Unable to locate /$os.css")
    stage.setScene(scene)

    // set our stage position based on the values specified in editor config
    cfgMgr.editorConfig.value(EditorConfig.viewLeft) onValueNotify { x =>
      if (x >= 0) stage.setX(x)
    }
    cfgMgr.editorConfig.value(EditorConfig.viewTop) onValueNotify { y =>
      if (y >= 0) stage.setY(y)
    }
    // if geometry was specified on the command line, override the value from prefs
    geom.pos.foreach { pos => stage.setX(pos._1) ; stage.setY(pos._2) }

    // update our last "opened" time when a new editor is created
    Files.setLastModifiedTime(root, FileTime.fromMillis(System.currentTimeMillis))

    stage.show()
    editors.put(desk, epane)
    epane
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
