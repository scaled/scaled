//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.{Path, Paths}
import scaled._
import scaled.major.TextMode

/** Helper methods for creating test instances of things. */
object TestData {

  val log = new Logger {
    def log (msg :String) = println(msg)
    def log (msg :String, exn :Throwable) {
      println(msg)
      exn.printStackTrace(System.out)
    }
  }

  val exec = new Executor {
    val uiExec = new java.util.concurrent.Executor() {
      override def execute (op :Runnable) = op.run()
    }
    val bgExec = uiExec
  }

  val cwd = Paths.get("")
  val testScope = Config.Scope("test", cwd, None)

  val window :Window = new Window {
    val geometry = Geometry(100, 40, 10, 10)
    val frames = Seq()
    def focus = ???
    def workspace = TestData.workspace
    def close () {}
    def emitError (err :Throwable) = err.printStackTrace(System.err)
    def popStatus (msg :String, subtext :String) {
      println(msg)
      if (subtext != null) println(subtext)
    }
    def emitStatus (msg :String, ephemeral :Boolean) :Unit = println(msg)
    def clearStatus () {}
    val mini = new Minibuffer() {
      def apply[R] (mode :String, result :Promise[R], args :Any*) :Future[R] = result
    }
    def statusMini = mini
  }

  val workspace :Workspace = new Workspace {
    val name = "default"
    val root = Paths.get("")
    def editor = TestData.editor
    def config = editor.config
    val windows = Seq(window)
    val buffers = Seq()
    def createBuffer (name :String, state :List[State.Init[_]], reuse :Boolean) = buffer(name, "")
    def openBuffer (store :Store) = BufferImpl(store)
    def killBuffer (buffer :Buffer) {}
    def addHintPath (path :Path) {}
    def removeHintPath (path :Path) {}
    protected def log = TestData.log
  }

  val editor = new Editor {
    val config = new ConfigImpl("editor", cwd, testScope, EditorConfig :: Nil, None)
    def showURL (url :String) {}
  }

  val injector = new ServiceInjector(log, exec)
  val resolver = new ModeResolver(injector, null, null) {
    override protected def locate (major :Boolean, mode :String) = classOf[TextMode]
    override def configScope = testScope
    override protected def resolveConfig (mode :String, defs :List[Config.Defs]) =
      new ConfigImpl(mode, cwd, testScope, defs, None)
    override protected def injectInstance[T] (clazz :Class[T], args :List[Any]) =
      injector.injectInstance(clazz, args)
  }

  def env (view_ :RBufferView) = new Env {
    val msvc = injector
    val frame = null
    val window = TestData.window
    val view = view_
    val disp = null
    val mline = ModeLine.Noop
    def configScope = testScope
    def resolveConfig (mode :String, defs :List[Config.Defs]) =
      new ConfigImpl(mode, cwd, testScope, defs, None)
  }

  /** Creates a test buffer. For testing! */
  def buffer (name :String, text :String) = BufferImpl(new TextStore(name, "", text))
}
