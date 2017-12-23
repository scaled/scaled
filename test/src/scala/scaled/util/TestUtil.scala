//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

object TestUtil {

  def exec (rs :SeqBuffer[Runnable]) = new Scheduler() {
    override def execute (op :Runnable) :Unit = rs += op
    override def schedule (delay :Long, op :Runnable) = throw new UnsupportedOperationException()
  }
  class AccumExec private (rs :SeqBuffer[Runnable]) extends Executor(
    exec(rs), exec(rs), _.printStackTrace(System.err), None) {
    def this () = this(SeqBuffer[Runnable]())
    def executeAll () = {
      rs foreach { _.run() }
      rs.clear()
    }
  }
}
