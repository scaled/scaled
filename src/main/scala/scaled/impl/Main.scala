package scaled.impl

import com.google.common.collect.HashBiMap
import java.nio.file.{Path, Paths}
import java.util.concurrent.Executors
import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.stage.Stage
import reactual.Signal
import scala.collection.JavaConversions._
import scaled._
import scaled.util.Errors

class Main extends Application {

  private val pool = Executors.newFixedThreadPool(4) // TODO: config

  /** A signal emitted when a message is appended to the log. Because logging is app-global, but
    * buffers are associated with a particular editor pane, we just have every editor pane create a
    * *messages* buffer which appends anything it hears from the global log to itself. */
  val log = Signal[String]()

  val logger = new Logger {
    override def log (msg :String) :Unit = exec.runOnUI { Main.this.log.emit(msg) }
    override def log (msg :String, exn :Throwable) :Unit = exec.runOnUI {
      Main.this.log.emit(msg)
      Main.this.log.emit(Errors.stackTraceToString(exn))
    }
  }

  val editors = HashBiMap.create[String,EditorPane]()

  val server = new Server(this)
  val pkgMgr = new PackageManager(logger)
  val svcMgr = new ServiceManager(this)
  val cfgMgr = svcMgr.injectInstance(classOf[ConfigManager], Nil)

  val exec = new Executor {
    override val uiExec = new java.util.concurrent.Executor {
      override def execute (op :Runnable) = Platform.runLater(op)
    }
    override val bgExec = pool
  }

  /** Opens `path` in the editor pane associated with `workspace`. If no such editor pane exists one
    * is created. */
  def openInWorkspace (path :String, workspace :String) {
    val epane = editors.get(workspace) match {
      case null => createEditor(new Stage(), workspace, NoGeom)
      case epane => epane
    }
    println(s"Opening $workspace / $path")
    epane.visitPath(path)
  }

  /** Closes `epane`. If that was the last open editor pane, terminates Scaled. */
  def closeEditor (epane :EditorPane) {
    epane.dispose()
    epane.stage.close()
    editors.inverse.remove(epane)
    if (editors.isEmpty()) sys.exit(0) // TODO: cleanup?
  }

  case class Geom (size :Option[(Int,Int)], pos :Option[(Int,Int)])
  val NoGeom = Geom(None, None)

  override def start (stage :Stage) {
    val geom = Option(System.getProperty("geometry")).map(parseGeometry).getOrElse(NoGeom)
    val epane = createEditor(stage, System.getProperty("scaled.workspace", "default"), geom)
    // open a pane/tab for each file passed on the command line
    getParameters.getRaw foreach epane.visitPath

    // start our command server
    server.start()

    // now that our main window is created, we can tweak the quit menu shortcut key
    tweakQuitMenuItem()
  }

  private def parseGeometry (geom :String) :Geom = geom.split("[x+]") match {
    case Array(w, h, x, y) => Geom(Some(w.toInt -> h.toInt), Some(x.toInt -> y.toInt))
    case Array(w, h)       => Geom(Some(w.toInt -> h.toInt), None)
    case _                 => Geom(None, None)
  }

  private def createEditor (stage :Stage, workspace :String, geom :Geom) :EditorPane = {
    val epane = new EditorPane(this, stage) {
      override def bufferSize = geom.size getOrElse super.bufferSize
    }
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
    editors.put(workspace, epane)
    epane
  }

  // tweaks the shortcut on the quit menu to avoid conflict with M-q
  private def tweakQuitMenuItem () :Unit = try {
    import com.sun.glass._
    val app = ui.Application.GetApplication
    val getAppleMenu = app.getClass.getMethod("getAppleMenu")
    if (getAppleMenu != null) {
      getAppleMenu.setAccessible(true)
      val menu = getAppleMenu.invoke(app).asInstanceOf[ui.Menu]
      val items = menu.getItems
      val quit = items.get(items.size-1).asInstanceOf[ui.MenuItem]
      quit.setShortcut('q', events.KeyEvent.MODIFIER_COMMAND|events.KeyEvent.MODIFIER_SHIFT)
    }

  } catch {
    case t :Throwable =>
      println("Failed to tweak Quit menu item")
      t.printStackTrace(System.err)
  }
}

object Main {

  def main (args :Array[String]) {
    Application.launch(classOf[Main], args :_*)
  }
}
