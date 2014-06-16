//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._

class MutableLineTest {

  @Test def insertDeleteReplace () {
    val buf = TestData.buffer("test", "")
    val line = new MutableLine(buf, "Every good boy deserves fudge.".toCharArray)
    line.insert(Loc(0, line.asString.indexOf("fudge")), Line("tasty "))
    assertEquals("Every good boy deserves tasty fudge.", line.asString)
    line.delete(Loc(0, line.length-1), 1)
    assertEquals("Every good boy deserves tasty fudge", line.asString)
    line.insert(Loc(0, line.length), '!', Styles.None, Syntax.Default)
    assertEquals("Every good boy deserves tasty fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("tasty")), 5, Line("lots of"))
    assertEquals("Every good boy deserves lots of fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("lots of")), 7, Line("some"))
    assertEquals("Every good boy deserves some fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("deserves")), 8, Line("requires"))
    assertEquals("Every good boy requires some fudge!", line.asString)
    // TODO: boundary conditions?
  }

  @Test def slicing () {
    val buf = TestData.buffer("test", "")
    val line = new MutableLine(buf, "Every good boy deserves fudge.".toCharArray)
    assertEquals("good", line.slice(6, 6+4).asString)
    // TODO: boundary conditions?
  }
}
