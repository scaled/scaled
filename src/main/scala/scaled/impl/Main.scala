package scaled.impl

import com.google.common.collect.HashBiMap
import java.io.File
import java.util.concurrent.Executors
import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.Stage
import reactual.Signal
import scala.collection.JavaConversions._
import scaled.EditorConfig

class Main extends Application with Logger {

  /** An executor service for great concurrency. */
  val exec = Executors.newFixedThreadPool(4) // TODO: config

  // locate and create our metadata dir
  val homeDir = new File(System.getProperty("user.home"))
  val metaDir = Filer.requireDir(locateMetaDir)
  val editors = HashBiMap.create[String,EditorPane]()

  val server = new Server(this)
  val watchMgr = new WatchManager()
  val pkgMgr = new pkg.PackageManager(this)
  val cfgMgr = new ConfigManager(this)
  val svcMgr = new ServiceManager(this)

  /** A signal emitted when a message is appended to the log. Because logging is app-global, but
    * buffers are associated with a particular editor pane, we just have every editor pane create a
    * *messages* buffer which appends anything it hears from the global log to itself. */
  val log = Signal[String]()

  /** Opens `path` in the editor pane associated with `workspace`. If no such editor pane exists one
    * is created. */
  def openInWorkspace (path :String, workspace :String) {
    val epane = editors.get(workspace) match {
      case null => createEditor(new Stage(), workspace)
      case epane => epane
    }
    println(s"Opening $workspace / $path")
    epane.visitPath(path)
  }

  /** Closes `epane`. If that was the last open editor pane, terminates Scaled. */
  def closeEditor (epane :EditorPane, ecode :Int) {
    epane.dispose()
    epane.stage.close()
    editors.inverse.remove(epane)
    if (editors.isEmpty()) sys.exit(ecode) // TODO: cleanup?
  }

  override def log (msg :String) :Unit = log.emit(msg)
  override def log (msg :String, exn :Throwable) {
    log.emit(msg)
    log.emit(Utils.stackTraceToString(exn))
    // TODO: if in devel mode, record these to system.err?
  }

  override def start (stage :Stage) {
    val epane = createEditor(stage, System.getProperty("scaled.workspace", "default"))
    // open a pane/tab for each file passed on the command line
    getParameters.getRaw foreach epane.visitPath

    // start our command server
    server.start()

    // now that our main window is created, we can tweak the quit menu shortcut key
    tweakQuitMenuItem()
  }

  private def createEditor (stage :Stage, workspace :String) :EditorPane = {
    val epane = new EditorPane(this, stage)
    val scene = new Scene(epane)
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    cfgMgr.editorConfig.value(EditorConfig.viewLeft) onValueNotify { x =>
      if (x >= 0) stage.setX(x)
    }
    cfgMgr.editorConfig.value(EditorConfig.viewTop) onValueNotify { y =>
      if (y >= 0) stage.setY(y)
    }
    stage.setScene(scene)
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

  // TODO: platform specific app dirs
  private def locateMetaDir :File =
    (homeDir /: Seq("Library", "Application Support", "Scaled"))(new File(_, _))
}

object Main {

  def main (args :Array[String]) {
    Application.launch(classOf[Main], args :_*)
  }
}
