//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class BufferTest {

  val text = Seq("This is some text. Text is very interesting.",
                 "In this case we want to be sure the find fns work",
                 "and also don't match erroneously.")
  val buffer = Buffer("test", text.map(Line.apply))

  @Test def testFindForward () :Unit = {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findForward(mp, buffer.start))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) :Unit = {
      assertEquals(mloc, buffer.findForward(mm, buffer.start))
      // make sure a search for "match" exactly on match does match
      assertEquals(mloc, buffer.findForward(mm, mloc))
      // make sure a search for "match" just after match doesn't match
      assertEquals(Loc.None, buffer.findForward(mm, mloc.nextC))
    }
    test(Matcher.exact("match"))
    test(Matcher.regexp("\\bmatch\\b"))
  }

  @Test def testFindBackward () :Unit = {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findBackward(mp, buffer.end))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) :Unit = {
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
