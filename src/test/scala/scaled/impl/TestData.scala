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

  val editor = new Editor {
    def showURL (url :String) {}
    def defer (op :Runnable) = op.run()
    def mini[R] (mode :String, result :Promise[R], args :Any*) :Future[R] = result
    def emitStatus (msg :String) = println(msg)
    def clearStatus () {}
    def exit (code :Int) {}
    def buffers = Seq()
    def openBuffer (buffer :String) {}
    def visitFile (file :File) = null
    def killBuffer (buffer :String) = false
  }

  val config = new ConfigImpl("editor", None)

  val resolver = new ModeResolver(editor, config) {
    override protected def locate (major :Boolean, mode :String) = Future.success(classOf[TextMode])
  }

  /** Creates a test buffer. For testing! */
  def buffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))
}
