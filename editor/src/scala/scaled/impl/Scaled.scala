//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.awt.Desktop
import java.io.File
import java.nio.file.{Path, Paths}
import java.util.concurrent.Executors
import java.util.{List => JList, ArrayList, Timer, TimerTask}
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.{Application, Platform}
import javafx.event.{ActionEvent, EventHandler}
import javafx.stage.Stage
import javafx.util.Duration
import scaled._
import scaled.util.Errors

class Scaled extends Application with Editor {
  private val pool = Executors.newFixedThreadPool(4) // TODO: config

  /** A signal emitted when a message is appended to the log. Because logging is app-global, but
    * buffers are associated with a particular editor pane, we just have every editor pane create a
    * *messages* buffer which appends anything it hears from the global log to itself. */
  val log = Signal[String]()

  val logger = new Logger {
    override def log (msg :String) :Unit = Platform.runLater(new Runnable() {
      override def run = doLog(msg)
    })
    override def log (msg :String, exn :Throwable) :Unit = Platform.runLater(new Runnable() {
      override def run = { doLog(msg) ; doLog(Errors.stackTraceToString(exn)) }
    })
    private def doLog (msg :String) {
      debugLog(msg)
      Scaled.this.log.emit(msg)
    }
  }

  val timer = new Timer("Scaled Timer", true)

  val uiScheduler = new Scheduler {
    override def execute (op :Runnable) = Platform.runLater(op)
    override def schedule (delay :Long, op :Runnable) = {
      var canceled = false
      new Timeline(new KeyFrame(Duration.millis(delay), new EventHandler[ActionEvent]() {
        override def handle (event :ActionEvent) = if (!canceled) op.run()
      })).play()
      Closeable({ canceled = true })
    }
  }
  val bgScheduler = new Scheduler() {
    override def execute (op :Runnable) = pool.execute(op)
    override def schedule (delay :Long, op :Runnable) = {
      val task = new TimerTask() { override def run () = op.run() }
      timer.schedule(task, delay)
      Closeable({ task.cancel() })
    }
  }
  private def handleError (err :Throwable) = logger.log(Errors.stackTraceToString(err))
  override val exec = new Executor(uiScheduler, bgScheduler, handleError, Some(pool))

  val server = new Server(this)
  val pkgMgr = new PackageManager(logger)
  val wspMgr = new WorkspaceManager(this)
  val svcMgr = new ServiceManager(this)
  val cfgMgr = svcMgr.injectInstance(classOf[ConfigManager], Nil)

  val configScope = Config.Scope("global", pkgMgr.metaDir, None)
  state[Config.Scope]() = configScope

  /** If debug logging is enabled, writes `msg` to console, otherwise noops. */
  val debugLog = if (java.lang.Boolean.getBoolean("scaled.debug")) (msg :String) => println(msg)
                 else (msg :String) => ()

  override def start (stage :Stage) {
    // round up exceptions thrown from runnables sent to Platform.runLater
    Thread.currentThread.setUncaughtExceptionHandler((thread, err) => handleError(err))
    // we have to defer resolution of auto-load services until the above constructors have
    // completed; always there are a twisty maze of initialization dependencies
    svcMgr.resolveAutoLoads()
    // create the starting editor and visit therein the starting files
    val argvFiles = Seq.view(getParameters.getRaw) map wspMgr.resolve
    wspMgr.visit(stage, argvFiles ++ Scaled.openFiles())
    // listen for open files events
    Scaled.openFiles.via(uiScheduler).onValue { _.foreach(wspMgr.visit) }
    // start our command server
    server.start()
  }

  override def stop () {
    svcMgr.shutdown();
    pool.shutdown()
  }

  //
  // Editor API

  override val workspaceOpened = Utils.safeSignal[Workspace](logger)
  override def config = cfgMgr.editorConfig(configScope)
  override def showURL (url :String) = {
    // getHostSevices.showDocument is very crashy on Mac OS X right now, so avoid it
    if (System.getProperty("os.name") != "Mac OS X") getHostServices.showDocument(url)
    else Runtime.getRuntime.exec(Array("open", url))
  }
}

object Scaled {

  /** The port on which [Server] listens for commands. */
  final val Port = (Option(System.getenv("SCALED_PORT")) getOrElse "32323").toInt

  /** Files received via 'open files' events. These may arrive long before JavaFX is ready to run,
    * so we register a handler immediately in main() and buffer files in this reactive value. This
    * catches the event that comes in when a macOS app is opened with initial files. */
  val openFiles = Value(Seq[Path]())

  def main (args :Array[String]) {
    try {
      Desktop.getDesktop.setOpenFileHandler(
        // we force an update here because the last open files request may be the exact same path
        // in which case we still want to overwrite it and notify the listener; we could clear the
        // value out after it's processed, but this is more expedient; rigor!
        event => openFiles.updateForce(Seq.view(event.getFiles).map(_.toPath))
      )
    } catch {
      case uoe :UnsupportedOperationException => // oh well
      case t :Throwable => t.printStackTrace(System.err)
    }
    // if there's already a Scaled instance running, pass our args to it and exit; otherwise launch
    // our fully operational mothership
    if (!sendFiles(args)) Application.launch(classOf[Scaled], args :_*)
  }

  private def sendFiles (args :Array[String]) :Boolean = {
    import java.io.{IOException, OutputStreamWriter, PrintWriter}
    import java.net.{ConnectException, Socket, InetSocketAddress}

    try {
      val sock = new Socket()
      sock.connect(new InetSocketAddress("localhost", Port), 2000)
      val out = new PrintWriter(new OutputStreamWriter(sock.getOutputStream(), "UTF-8"))
      val cwd = Paths.get(System.getProperty("user.dir"))
      args foreach { file => out.println(s"open ${cwd.resolve(Paths.get(file))}") }
      out.close()
      sock.close()
      true
    } catch {
      case e :ConnectException => false
      case e :Throwable => e.printStackTrace(System.err) ; false
    }
  }
}
