//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.buffer

import java.io.{File, StringReader}

import org.junit._
import org.junit.Assert._

class BufferTest {

  @Test def testBasics () {
    val text = """Who was that man?
      |Now is the time for all good citizens to come to the aid of their country.
      |Every good boy deserves fudge.
      |The quick brown fox jumped over the lazy dog.""".stripMargin
    val buffer = Buffer("test", new File(""), new StringReader(text))
    assertEquals(4, buffer.lines.size)
  }
}
