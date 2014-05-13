//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.File
import org.junit._
import org.junit.Assert._

object BufferTest {

  /** Creates a read-only buffer view for testing. */
  def bufferV (_name :String, _lines :Seq[LineV]) :BufferV = new BufferV() {
    def name = _name
    def file = new File(name)
    def mark = None
    def dirty = false
    def lines = _lines
    val maxLineLength = _lines.map(_.length).max
  }
}

class BufferTest {
  import BufferTest._

  val text = Seq("This is some text. Text is very interesting.",
                 "In this case we want to be sure the find fns work",
                 "and also don't match erroneously.")
  val buffer = bufferV("test", text.map(Line.apply))

  @Test def testFindForward () {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findForward(mp, buffer.start))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) {
      assertEquals(mloc, buffer.findForward(mm, buffer.start))
      // make sure a search for "match" exactly on match does match
      assertEquals(mloc, buffer.findForward(mm, mloc))
      // make sure a search for "match" just after match doesn't match
      assertEquals(Loc.None, buffer.findForward(mm, mloc.nextC))
    }
    test(Matcher.exact("match"))
    test(Matcher.regexp("\\bmatch\\b"))
  }

  @Test def testFindBackward () {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findBackward(mp, buffer.end))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) {
      assertEquals(s"findBackward($mm) matches", mloc, buffer.findBackward(mm, buffer.end))
      // make sure a search for "match" exactly on match does not match (findBackward starts
      // immediately prior to `start`)
      assertEquals(s"findBackward($mm) at match doesn't match",
                   Loc.None, buffer.findBackward(mm, mloc))
      // make sure a search for "match" before match also doesn't match
      assertEquals(s"findBackward($mm) at match,prevC doesn't match",
                   Loc.None, buffer.findBackward(mm, mloc.prevC))
    }
    test(Matcher.exact("match"))
    test(Matcher.regexp("\\bmatch\\b"))
  }
}
