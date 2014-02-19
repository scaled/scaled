//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.{File, StringReader}
import java.util.Arrays

import org.junit._
import org.junit.Assert._

class LineTest {

  @Test def insertDeleteReplace () {
    val buf = TestData.buffer("test", "")
    val line = new LineImpl("Every good boy deserves fudge.".toCharArray, buf)
    line.insert(line.asString.indexOf("fudge"), "tasty ")
    assertEquals("Every good boy deserves tasty fudge.", line.asString)
    line.delete(line.length-1, 1)
    assertEquals("Every good boy deserves tasty fudge", line.asString)
    line.insert(line.length, '!')
    assertEquals("Every good boy deserves tasty fudge!", line.asString)
    line.replace(line.asString.indexOf("tasty"), 5, "lots of".toCharArray)
    assertEquals("Every good boy deserves lots of fudge!", line.asString)
    line.replace(line.asString.indexOf("lots of"), 7, "some".toCharArray)
    assertEquals("Every good boy deserves some fudge!", line.asString)
    line.replace(line.asString.indexOf("deserves"), 8, "requires".toCharArray)
    assertEquals("Every good boy requires some fudge!", line.asString)
    // TODO: boundary conditions?
  }

  @Test def slicing () {
    val buf = BufferImpl("test", new File(""), new StringReader(""))
    val line = new LineImpl("Every good boy deserves fudge.".toCharArray, buf)
    assertTrue(Arrays.equals("good".toCharArray, line.slice(6, 6+4)))
    assertEquals("good", line.sliceString(6, 6+4))
    // TODO: boundary conditions?
  }
}
