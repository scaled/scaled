//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._

class MutableLineTest {

  @Test def insertDeleteReplace () :Unit = {
    val buf = TestData.buffer("test", "")
    val line = new MutableLine(buf, "Every good boy deserves fudge.".toCharArray)
    line.insert(Loc(0, line.asString.indexOf("fudge")), Line("tasty "))
    assertEquals("Every good boy deserves tasty fudge.", line.asString)
    line.delete(Loc(0, line.length-1), 1)
    assertEquals("Every good boy deserves tasty fudge", line.asString)
    line.insert(Loc(0, line.length), '!', Syntax.Default)
    assertEquals("Every good boy deserves tasty fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("tasty")), 5, Line("lots of"))
    assertEquals("Every good boy deserves lots of fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("lots of")), 7, Line("some"))
    assertEquals("Every good boy deserves some fudge!", line.asString)
    line.replace(Loc(0, line.asString.indexOf("deserves")), 8, Line("requires"))
    assertEquals("Every good boy requires some fudge!", line.asString)
    // TODO: boundary conditions?
  }

  @Test def slicing () :Unit = {
    val buf = TestData.buffer("test", "")
    val line = new MutableLine(buf, "Every good boy deserves fudge.".toCharArray)
    assertEquals("good", line.slice(6, 6+4).asString)
    // TODO: boundary conditions?
  }

  case class TestTag (val text :String) extends Line.Tag
  case class EphTestTag (val text :String) extends Line.Tag {
    override def ephemeral = true
  }

  @Test def testLineTags () :Unit = {
    val buf = TestData.buffer("test", "")
    val line = new MutableLine(buf, "Every good boy deserves fudge.".toCharArray)
    val ltags = line.lineTagSet

    val tag = TestTag("one") ; val dtag = TestTag("two")
    ltags.set(tag)
    assertEquals(tag, line.lineTag(dtag))
    ltags.clear(classOf[TestTag])
    assertEquals(dtag, line.lineTag(dtag))

    // now set the non-ephemeral tag again and make sure it doesn't go away on edits
    ltags.set(tag)

    // make sure an ephemeral tag *does* go away on edits
    val etag = EphTestTag("one") ; val detag = EphTestTag("two")
    ltags.set(etag)
    line.insert(Loc(0, 0), ' ', Syntax.Default)
    assertEquals(detag, line.lineTag(detag))
    assertEquals(tag, line.lineTag(dtag))

    ltags.set(EphTestTag("one"))
    line.delete(Loc(0, 0), 2)
    assertEquals(detag, line.lineTag(detag))
    assertEquals(tag, line.lineTag(dtag))

    ltags.set(EphTestTag("one"))
    line.replace(Loc(0, 0), 2, Line("yay"))
    assertEquals(detag, line.lineTag(detag))
    assertEquals(tag, line.lineTag(dtag))
  }
}
