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

  val immSched = new Scheduler() {
    override def execute (op :Runnable) = op.run()
    override def schedule (delay :Long, op :Runnable) = throw new UnsupportedOperationException()
  }
  val exec = new Executor(immSched, immSched, _.printStackTrace(System.err), None)

  val cwd = Paths.get("")
  val testScope = Config.Scope("test", cwd, None)

  val editor = new Editor {
    val config = new ConfigImpl("editor", cwd, testScope, EditorConfig :: Nil, None)
    val exec = TestData.exec
    val workspaceOpened = Signal[Workspace]()
    val clipboard = new Clipboard {
      private var contents :Option[String] = None
      def read = contents
      def set (text :String) { contents = Some(text) }
    }
    def showURL (url :String) {}
  }

  val workspace :Workspace = new Workspace {
    val name = "default"
    val root = Paths.get("")
    def editor = TestData.editor
    def config = editor.config
    val windows = Seq(window)
    val buffers = Seq()
    val bufferOpened = Signal[RBuffer]()
    def createBuffer (store :Store, state :List[State.Init[_]], reuse :Boolean) = BufferImpl(store)
    def openBuffer (store :Store) = BufferImpl(store)
    def openWindow (geom :Option[Geometry]) = null
    def getInfoWindow (tag :String) = null
    def visitWindowConfig (window :Window) {}
    def killBuffer (buffer :Buffer) {}
    def addHintPath (path :Path) {}
    def removeHintPath (path :Path) {}
    def exec = editor.exec
    protected def log = TestData.log
  }

  val window :Window = new Window {
    val geometry = Geometry(100, 40, 10, 10)
    val frames = Seq()
    val onClose = Signal[Window]()
    def focus = ???
    def workspace = TestData.workspace
    def close () {}
    def buffers = Seq()
    def exec = workspace.exec
    def popStatus (msg :String, subtext :String) {
      println(msg)
      if (subtext != null) println(subtext)
    }
    def emitStatus (msg :String, ephemeral :Boolean) :Unit = println(msg)
    def clearStatus () {}
    val mini = new Minibuffer() {
      def apply[R] (mode :String, args :Any*) :Future[R] = Promise[R]()
    }
    def statusMini = mini
    def toFront {}
  }

  val injector = new ServiceInjector(log, exec, editor) {
    private val stockSvcs = Map[Class[_],Class[_]](
      classOf[ConfigService] -> classOf[ConfigManager],
      classOf[WatchService]  -> classOf[WatchManager])
    override protected def resolveService (sclass :Class[_]) = stockSvcs.get(sclass).fold(
      super.resolveService(sclass))(injectInstance(_, Nil).asInstanceOf[AbstractService])
  }
  val resolver = new ModeResolver(injector, null, null) {
    override protected def locate (major :Boolean, mode :String) = classOf[TextMode]
    override def configScope = testScope
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
