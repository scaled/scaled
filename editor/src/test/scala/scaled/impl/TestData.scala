//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.{File, StringReader}

import reactual.{Future, Promise}

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
    def runOnUI (op :Runnable) = op.run()
    def runInBackground (op :Runnable) = op.run()
  }

  val editor = new Editor {
    def exit () {}
    def showURL (url :String) {}
    def popStatus (msg :String, subtext :String) {
      println(msg)
      if (subtext != null) println(subtext)
    }
    def emitStatus (msg :String, ephemeral :Boolean) :Unit = println(msg)
    def emitError (err :Throwable) = err.printStackTrace(System.err)
    def clearStatus () {}
    def mini[R] (mode :String, result :Promise[R], args :Any*) :Future[R] = result
    def buffers = Seq()
    def openBuffer (buffer :String) = null
    def createBuffer (buffer :String, mode :String, reuse :Boolean) = null
    def visitFile (file :File) = null
    def visitConfig (name :String) = null
    def killBuffer (buffer :String) = false
  }

  val config = new ConfigImpl("scaled", EditorConfig :: Nil, None)

  val injector = new ServiceInjector()
  val resolver = new ModeResolver(log, exec, editor) {
    override protected def locate (major :Boolean, mode :String) = classOf[TextMode]
    override protected def resolveConfig (mode :String, defs :List[Config.Defs]) =
      modeConfig(mode, defs)
    override protected def injectInstance[T] (clazz :Class[T], args :List[Any]) =
      injector.injectInstance(clazz, args)
  }

  def modeConfig (mode :String, defs :List[Config.Defs]) = new ConfigImpl(mode, defs, Some(config))

  def env (view_ :RBufferView) = new Env {
    val log = TestData.log
    val exec = TestData.exec
    val editor = TestData.editor
    val view = view_
    val disp = null
    def resolveConfig (mode :String, defs :List[Config.Defs]) = modeConfig(mode, defs)
  }

  /** Creates a test buffer. For testing! */
  def buffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))
}
