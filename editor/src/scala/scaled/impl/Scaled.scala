//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import java.nio.file.{Path, Paths}
import java.util.concurrent.Executors
import java.util.{List => JList, ArrayList}
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.{Application, Platform}
import javafx.event.{ActionEvent, EventHandler}
import javafx.stage.Stage
import javafx.util.Duration
import scaled._
import scaled.platform.OpenFilesHelper
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

  override val exec = new Executor {
    override val uiExec = new java.util.concurrent.Executor {
      override def execute (op :Runnable) = Platform.runLater(op)
    }
    override val bgExec = pool
    override def bgExecService = pool
    override val errHandler = new Executor.ErrorHandler() {
      def emitError (err :Throwable) = logger.log(Errors.stackTraceToString(err))
    }
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
  val cfgMgr = svcMgr.injectInstance(classOf[ConfigManager], Nil)

  val configScope = Config.Scope("global", pkgMgr.metaDir, None)
  state[Config.Scope]() = configScope

  /** If debug logging is enabled, writes `msg` to console, otherwise noops. */
  val debugLog = if (java.lang.Boolean.getBoolean("scaled.debug")) (msg :String) => println(msg)
                 else (msg :String) => ()

  override def start (stage :Stage) {
    // we have to defer resolution of auto-load services until the above constructors have
    // completed; always there are a twisty maze of initialization dependencies
    svcMgr.resolveAutoLoads()
    // listen for open files events
    OpenFilesHelper.setListener(files => onMainThread { files.foreach(wspMgr.visit) })
    // create the starting editor and visit therein the starting files
    val argvFiles = Seq.view(getParameters.getRaw) map wspMgr.resolve
    wspMgr.visit(stage, argvFiles ++ OpenFilesHelper.launchFiles)
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

  def main (args :Array[String]) {
    // if we're running on Mac, wire up an open files handler; this has to happen immediately or
    // we'll miss events delivered right after startup
    OpenFilesHelper.init()
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
