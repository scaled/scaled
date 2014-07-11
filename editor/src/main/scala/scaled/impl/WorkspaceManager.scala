//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.collect.HashBiMap
import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import java.util.{List => JList}
import javafx.application.Platform
import javafx.scene.Scene
import javafx.stage.Stage
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.io.Source
import scaled._
import scaled.pacman.Filez
import scaled.util.Errors

/** Manages workspaces, and the creation of editors therein. */
class WorkspaceManager (app :Scaled) extends AbstractService with WorkspaceService {
  import scala.collection.convert.WrapAsScala._

  private def log = app.logger
  // the directory in which we store workspace metadata
  private val wsdir = Files.createDirectories(app.pkgMgr.metaDir.resolve("Workspaces"))
  // a map from desktop name to last used workspace
  private val lastws = MMap[String,String]() // TODO: read from persistent store
  // currently resolved workspaces
  private var wscache = CacheBuilder.newBuilder.build(new CacheLoader[String,WorkspaceImpl]() {
    override def load (name :String) = new WorkspaceImpl(
      app, WorkspaceManager.this, name, wsdir.resolve(name))
  })

  /** Creates the starting workspace and editor and opens `paths` therein. */
  def visit (stage :Stage, paths :JList[String]) {
    val desktop = curDesktop
    val epane = workspaceFor(desktop).open(desktop, stage)
    paths foreach epane.visitPath
  }

  /** Visits `path` in the editor on the desktop detected to be current.
    * If no such editor pane exists one is created. */
  def visit (path :String) {
    val desktop = curDesktop
    workspaceFor(desktop).open(desktop).visitPath(path)
  }

  def checkExit () {
    // if no workspaces have editors open, it's time to go
    if (wscache.asMap.values.forall(_.editors.isEmpty)) Platform.exit()
  }

  override def list = try {
    Files.list(wsdir).collect(Collectors.toList[Path]).filter(Files.isDirectory(_)).
      map(_.getFileName.toString)
  } catch {
    case e :IOException => log.log("Failed to list $wsdir", e) ; Nil
  }

  override def create (wsname :String) {
    val root = wsdir.resolve(wsname)
    if (Files.exists(root)) throw Errors.feedback(s"Workspace named $wsname already exists.")
    Files.createDirectories(root)
    open(wsname)
  }

  override def open (wsname :String) {
    val root = wsdir.resolve(wsname)
    if (!Files.exists(root)) throw Errors.feedback(s"No workspace named $wsname")
    val desktop = curDesktop
    openOn(wsname, desktop).open(desktop).stageToFront()
  }

  override def didStartup () {} // unused
  override def willShutdown () {} // unused

  private val curDesktop :String = System.getProperty("scaled.curdesk", "") match {
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

  private def openOn (wsname :String, desktop :String) :WorkspaceImpl = {
    lastws.put(desktop, wsname)
    wscache.get(wsname)
    // TODO: save lastws to persistent store
  }

  /** Returns the workspace that should be used for the specified desktop. If a workspace is already
    * open on that desktop, it is returned. Otherwise, the workspace most recently used on that
    * desktop is (opened if necessary and) returned. Otherwise the default workspace is (created if
    * necessary, opened if necessary, and) returned. */
  private def workspaceFor (desktop :String) :WorkspaceImpl = {
    def inUse = wscache.asMap.values.find(_.editors.containsKey(desktop))
    def lastUsed = lastws.get(desktop).map(wscache.get)
    inUse orElse lastUsed getOrElse  {
      Files.createDirectories(wsdir.resolve(Workspace.DefaultName))
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
