//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import org.junit._
import org.junit.Assert._
import scaled._

class ProcessTest {
  import TestUtil._

  @Test def testOverflowBuffer () {
    val buffer = new Process.RollingBuffer(500)
    for (ii <- 0 to 1000) {
      buffer.append(s"Line $ii")
    }
    val out = buffer.toSeq
    assertEquals(501, out.size)
    assertEquals("Line 0", out.head)
    assertEquals("Line 1000", out.last)
  }

  @Test def testShort () {
    val buffer = new Process.RollingBuffer(500)
    for (ii <- 0 to 100) {
      buffer.append(s"Line $ii")
    }
    val out = buffer.toSeq
    assertEquals(101, out.size)
    assertEquals("Line 0", out.head)
    assertEquals("Line 100", out.last)
  }

  @Test def testReallyShort () {
    val buffer = new Process.RollingBuffer(500)
    for (ii <- 0 to 10) {
      buffer.append(s"Line $ii")
    }
    val out = buffer.toSeq
    assertEquals(11, out.size)
    assertEquals("Line 0", out.head)
    assertEquals("Line 10", out.last)
  }

  @Test def testExec () {
    val exec = new AccumExec()
    Process.exec(exec, Seq("ls", "/")).onSuccess(res => {
      assertEquals(0, res.exitCode)
      assertTrue(res.stdout.size > 0)
      assertEquals(0, res.stderr.size)
    })
    exec.executeAll()
  }
}
