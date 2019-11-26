//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class PackageTest {

  class TestCloseable extends AutoCloseable {
    var closed = false
    var twiddle = 0
    def close () :Unit = { closed = true }
  }

  class FailingCloseable extends AutoCloseable {
    var twiddle = 0
    def close () = throw new Exception("Oops!")
  }

  @Test def testUsing () :Unit = {
    // test normal close behavior
    val tc0 = using(new TestCloseable()) { tc =>
      tc.twiddle += 1
      tc
    }
    assertEquals(1, tc0.twiddle)
    assertTrue(tc0.closed)

    // test that close happens even if an exception is thrown
    val tc1 = new TestCloseable()
    try {
      using(tc1) { tc =>
        tc.twiddle += 1
        throw new Exception("Zoiks!")
        tc.twiddle += 1
      }
      fail("unreached")
    } catch {
      case e :Exception =>
        assertEquals("Zoiks!", e.getMessage)
        assertEquals(1, tc1.twiddle)
        assertTrue(tc1.closed)
    }

    // test that nothing bad happens if an exception is thrown creating the closeable
    def getTC :TestCloseable = throw new Exception("Oops!")
    try {
      using(getTC) { tc =>
        tc.twiddle += 1
      }
      fail("unreached")
    } catch {
      case e :Exception =>
        assertEquals("Oops!", e.getMessage)
    }

    // test that exceptions thrown during close are propagated
    val fc0 = new FailingCloseable()
    try {
      using(fc0) { fc =>
        fc.twiddle += 1
      }
    } catch {
      case e :Exception =>
        assertEquals("Oops!", e.getMessage)
        assertEquals(1, fc0.twiddle)
    }

    // test that exceptions thrown during close are appended to in flight exceptions
    val fc1 = new FailingCloseable()
    try {
      using(fc1) { fc =>
        fc.twiddle += 1
        throw new Exception("Zoiks!")
      }
    } catch {
      case e :Exception =>
        assertEquals("Zoiks!", e.getMessage)
        assertEquals("Oops!", e.getSuppressed()(0).getMessage)
        assertEquals(1, fc1.twiddle)
    }
  }
}
