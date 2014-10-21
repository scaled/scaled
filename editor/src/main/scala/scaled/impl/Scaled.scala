//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.{Path, Paths}
import java.util.concurrent.Executors
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.{Application, Platform}
import javafx.event.{ActionEvent, EventHandler}
import javafx.stage.Stage
import javafx.util.Duration
import scala.collection.JavaConversions._
import scaled._
import scaled.util.Errors

class Scaled extends Application {

  private val pool = Executors.newFixedThreadPool(4) // TODO: config

  /** A signal emitted when a message is appended to the log. Because logging is app-global, but
    * buffers are associated with a particular editor pane, we just have every editor pane create a
    * *messages* buffer which appends anything it hears from the global log to itself. */
  val log = Signal[String]()

  val logger = new Logger {
    override def log (msg :String) :Unit = exec.runOnUI { doLog(msg) }
    override def log (msg :String, exn :Throwable) :Unit =
      exec.runOnUI { doLog(msg) ; doLog(Errors.stackTraceToString(exn)) }
    private def doLog (msg :String) {
      debugLog(msg)
      Scaled.this.log.emit(msg)
    }
  }

  val exec = new Executor {
    override val uiExec = new java.util.concurrent.Executor {
      override def execute (op :Runnable) = Platform.runLater(op)
    }
    override val bgExec = pool
    override def uiTimer (delay :Long) = {
      val result = Promise[Unit]()
      new Timeline(new KeyFrame(Duration.millis(delay), new EventHandler[ActionEvent]() {
        override def handle (event :ActionEvent) = result.succeed(())
      })).play()
      result
    }
  }

  val server = new Server(this)
  val pkgMgr = new PackageManager(logger)
  val wspMgr = new WorkspaceManager(this)
  val svcMgr = new ServiceManager(this)

  val state = new State(State.init(Config.Scope("global", pkgMgr.metaDir, None)))

  /** If debug logging is enabled, writes `msg` to console, otherwise noops. */
  val debugLog = if (java.lang.Boolean.getBoolean("scaled.debug")) (msg :String) => println(msg)
                 else (msg :String) => ()

  override def start (stage :Stage) {
    // create the starting editor and visit therein the files specified on the command line
    wspMgr.visit(stage, getParameters.getRaw)
    // start our command server
    server.start()
    // now that our main window is created, we can tweak the quit menu shortcut key
    tweakQuitMenuItem()
  }

  override def stop () {
    pool.shutdown()
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
    case nsme :NoSuchMethodException => // nothing to see here, move it along
    case t :Throwable =>
      println("Failed to tweak Quit menu item")
      t.printStackTrace(System.err)
  }
}

object Scaled {

  def main (args :Array[String]) {
    Application.launch(classOf[Scaled], args :_*)
  }
}
