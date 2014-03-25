//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class LineTest {

  @Test def testSearch () {
    val l1 = new Line("The quick brown fox jumps over the lazy dog.")
    def test (line :Line, needle :String, start :Int) {
      val s = line.asString ; val idx = s.indexOf(needle, start)
      assertEquals(s"offset (from $start) of '$needle' in '$s'", idx,
                   line.search(new Line(needle), start))
      // println(s"$needle IN $s FROM $start => $idx")
    }
    test(l1, "quick", 0)
    test(l1, "The", 0)
    test(l1, "The", 1)
    test(l1, "lazy dog.", 0)
    test(l1, "lazy dog.", 50)

    val l2 = l1.slice(10, 34)
    test(l2, "brown fox", 0)
    test(l2, "jumps", 0)
    test(l2, "brown", 1)
    test(l2, "over the", 0)
    test(l2, "lazy dog.", 0)
    test(l2, "lazy dog.", 50)
  }

  @Test def testMatches () {
    val l1 = new Line("The quick brown fox jumps over the lazy dog.")
    def test (line :Line, needle :String, start :Int) {
      val s = line.asString ; val matches = s.indexOf(needle, start) == start
      assertEquals(s"'$needle' matches (at $start) '$s'", matches,
                   line.matches(new Line(needle), start))
      // println(s"$needle MATCH $s AT $start => $matches")
    }
    test(l1, "quick", 4)
    test(l1, "The", 0)
    test(l1, "The", 1)
    test(l1, "lazy dog.", 0)
    test(l1, "lazy dog.", 35)
    test(l1, "lazy dog!", 35)

    val l2 = l1.slice(10, 34)
    test(l2, "brown fox", 0)
    test(l2, "jumps", 0)
    test(l2, "brown", 1)
    test(l2, "over the", 17)
    test(l2, "lazy dog.", 0)
    test(l2, "lazy dog.", 50)
  }
}
