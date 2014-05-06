//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.util.concurrent.Executor
import org.junit.Assert._
import org.junit._

class ExpecterTest {

  val immediate = new Executor() {
    def execute (r :Runnable) = r.run()
  }

  @Test def textExpecter () {
    val ex = new Expecter(immediate, "echo", "foo") {
      override def onOut (line :String) {
        assertEquals("foo", line)
      }
      override def onFailure (err :Exception) {
        err.printStackTrace(System.err)
      }
    }
    assertEquals(0, ex.waitFor())
  }
}
