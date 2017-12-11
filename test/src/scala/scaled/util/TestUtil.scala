//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

object TestUtil {

  class AccumExec extends Executor {
    private val rs = SeqBuffer[Runnable]()
    val ui = new Scheduler() {
      override def execute (op :Runnable) :Unit = rs += op
      override def schedule (delay :Long, op :Runnable) = throw new UnsupportedOperationException()
    }
    val bg = ui
    val errHandler = new Executor.ErrorHandler() {
      override def emitError (err :Throwable) = err.printStackTrace(System.err)
    }
    def executeAll () = {
      rs foreach { _.run() }
      rs.clear()
    }
  }
}
