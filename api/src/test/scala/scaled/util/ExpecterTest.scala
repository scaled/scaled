//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.util.concurrent.Executor
import org.junit.Assert._
import org.junit._
import scala.collection.mutable.ArrayBuffer

class ExpecterTest {

  class AccumExec extends Executor {
    private val rs = ArrayBuffer[Runnable]()
    def execute (r :Runnable) :Unit = rs += r
    def executeAll () = {
      rs foreach { _.run() }
      rs.clear()
    }
  }

  @Test def testUnexpected () {
    val exec = new AccumExec()
    val ex = new Expecter(exec, "echo", "foo") {
      override def onUnexpected (line :String, isErr :Boolean) {
        assertEquals("foo", line)
      }
      override def onFailure (exn :Exception) {
        fail(exn.toString)
      }
    }
    val rv = ex.waitFor()
    exec.executeAll()
    assertEquals(0, rv)
  }

  @Test def testOneLineInteraction () {
    val exec = new AccumExec()
    val ex = new Expecter(exec, "cat") {
      override def onUnexpected (line :String, isErr :Boolean) {
        fail(s"Unexpected $line $isErr")
      }
      override def onFailure (exn :Exception) {
        fail(exn.toString)
      }
    }
    ex.interact(Seq("hello"), (line, isErr) => {
      assertEquals("hello", line)
      true
    })
    ex.close()
    assertEquals(0, ex.waitFor())
    try exec.executeAll()
    catch {
      case t :Throwable => ex.kill() ; throw t
    }
    finally {
    }
  }

  @Test def testMultilineInteraction () {
    val exec = new AccumExec()
    val ex = new Expecter(exec, "tee", "test.txt") {
      override def onUnexpected (line :String, isErr :Boolean) {
        fail(s"Unexpected $line $isErr")
      }
      override def onFailure (exn :Exception) {
        fail(exn.toString)
      }
    }
    ex.interact(Seq("howdy", "world"), (line, isErr) => {
      if ("howdy" == line) false
      else {
        if ("world" != line) fail("Unexpected $line $isErr")
        true
      }
    })
    ex.close()
    assertEquals(0, ex.waitFor())
    exec.executeAll()
  }
}
