//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

object TestUtil {

  class AccumExec extends Executor {
    private val rs = SeqBuffer[Runnable]()
    val uiExec = new java.util.concurrent.Executor() {
      override def execute (op :Runnable) :Unit = rs += op
    }
    val bgExec = uiExec
    val errHandler = new Executor.ErrorHandler() {
      override def emitError (err :Throwable) = err.printStackTrace(System.err)
    }
    override def uiTimer (delay :Long) = Future.failure(new Exception("Not implemented"))
    def executeAll () = {
      rs foreach { _.run() }
      rs.clear()
    }
  }
}
