//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.Closeable
import java.util.PriorityQueue

import org.junit._
import org.junit.Assert._

class SignalTest {

  class TestScheduler extends Scheduler {
    class QOp (val when :Long, val op :Runnable) extends Comparable[QOp] with Closeable {
      var canceled = false
      def run () = if (!canceled) op.run()
      def compareTo (other :QOp) = when.compareTo(other.when)
      def close () { canceled = true }
    }
    var now = 0L
    val queue = new PriorityQueue[QOp]()

    def execute (op :Runnable) = queue.offer(new QOp(now, op))
    def schedule (delay :Long, op :Runnable) = {
      val qop = new QOp(now+delay, op)
      queue.offer(qop)
      qop
    }

    def advance (time :Long) {
      now += time
      def loop (next :QOp) :Unit = if (next != null && next.when <= now) {
        queue.poll().run()
        loop(queue.peek)
      }
      loop(queue.peek)
    }
  }

  @Test def testDebounce () {
    val signal = Signal[Int]()
    val emitted = Seq.builder[Int]()
    signal.onValue { v => emitted += v }

    val sched = new TestScheduler()
    val debounced = Seq.builder[Int]()
    val dconn = signal.debounce(50, sched).onValue { v => debounced += v }
    val debatched = Seq.builder[Seq[Int]]()
    val dbconn = signal.debounceBatched(50, sched).onValue { v => debatched += v }

    signal.emit(1)
    sched.advance(1)
    signal.emit(2)
    sched.advance(1)
    signal.emit(3)
    sched.advance(98)
    signal.emit(4)
    sched.advance(1)
    signal.emit(5)
    sched.advance(1)
    signal.emit(6)
    sched.advance(98)
    signal.emit(7)
    sched.advance(1)
    signal.emit(8)
    dconn.close() // make sure closing with pending values queued doesn't include them
    sched.advance(99)
    signal.emit(9)
    dbconn.close()
    sched.advance(100)

    assertEquals(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9), emitted.toSeq)
    assertEquals(Seq(3, 6), debounced.toSeq)
    assertEquals(Seq(Seq(1, 2, 3), Seq(4, 5, 6), Seq(7, 8)), debatched.toSeq)
  }
}
