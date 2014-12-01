//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class LineTest {

  @Test def testEquals () {
    val l1 = Line("The quick brown fox jumps over the lazy dog.")
    val l2 = Line("The quick brown fox jumps over the lazy dog.")
    val l3 = Line("The quick brown pig jumps over the lazy dog.")

    assertTrue(l1 == l2)
    assertFalse(l1 == l3)
    assertTrue(l1.view(20) == l2.view(20))
    assertTrue(l1.view(20) == l3.view(20))
    assertTrue(l1.view(5, 20) == l2.view(5, 20))
    assertFalse(l1.view(5, 20) == l3.view(5, 20))
  }

  @Test def testCompare () {
    // sort is lexical
    val a = Line("abc")
    val x = Line("xyz")
    assertTrue((a compare x) < 0)
    assertTrue((x compare a) > 0)

    // upper case sorts before lower case
    val l1 = Line("Every good boy deserves fudge.")
    val l2 = Line("every good boy deserves fudge.")
    assertTrue((l1 compare l2) < 0)
    assertTrue((l2 compare l1) > 0)
    assertEquals(0, l1 compareIgnoreCase l2)
    assertEquals(0, l2 compareIgnoreCase l1)

    // prefix sorts before longer line
    val short = Line("won")
    val long = Line("wonder")
    assertTrue((short compare long) < 0)
    assertTrue((long compare short) > 0)
  }

  @Test def testIndexOf () {
    //                 0123456789012345678901234567890123456789012
    val l1 = Line("The quick brown fox jumps over the lazy dog.")
    def test (line :Line, needle :String, start :Int) {
      val s = line.asString ; val idx = s.indexOf(needle, start)
      assertEquals(s"offset (from $start) of '$needle' in '$s'", idx,
                   line.indexOf(Matcher.exact(needle), start))
      // println(s"$needle IN $s FROM $start => $idx")
    }
    test(l1, "quick", 0)
    test(l1, "The", 0)
    test(l1, "The", 1)
    test(l1, "lazy dog.", 0)
    test(l1, "lazy dog.", 50)
    test(l1, "lazy dog..", 0)

    val l2 = l1.slice(10, 34) // "brown fox jumps over the "
    test(l2, "brown fox", 0)
    test(l2, "jumps", 0)
    test(l2, "brown", 1)
    test(l2, "over the", 0)
    test(l2, "lazy dog.", 0)
    test(l2, "lazy dog.", 50)
  }

  @Test def testLastIndexOf () {
    //                 012345678901234567890123456789
    val l1 = Line("A man, a plan, a canal, Panama!")
    def test (line :Line, needle :String, start :Int) {
      val s = line.asString ; val idx = s.lastIndexOf(needle, start)
      assertEquals(s"last offset (from $start) of '$needle' in '$s'", idx,
                   line.lastIndexOf(Matcher.exact(needle), start))
      // println(s"$needle IN $s FROM $start => $idx")
    }
    test(l1, "Panama", l1.length)
    test(l1, "canal", l1.length)
    test(l1, "canal", 16) // should not match
    test(l1, "an", l1.length) // match an in Panama
    test(l1, "an", 18) // match an in canal
    test(l1, "an", 17) // match an in plan

    val l2 = l1.slice(10, 21) // "lan, a cana"
    test(l2, "cana", l2.length)
    test(l2, "an", l2.length)
    test(l2, "an", 8)
    test(l2, "an", 7)
    test(l2, "lan", l2.length)
  }

  @Test def testMatches () {
    val l1 = Line("The quick brown fox jumps over the lazy dog.")
    def test (line :Line, needle :String, start :Int) {
      val s = line.asString ; val matches = s.indexOf(needle, start) == start
      assertEquals(s"'$needle' matches (at $start) '$s'", matches,
                   line.matches(Matcher.exact(needle), start))
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

  @Test def testRegexpMatcher () {
    val t1 = "The quick brown fox jumps over the lazy dog."
    val l1 = Line(t1)
    assertEquals(t1.indexOf("quick"), l1.indexOf(Matcher.regexp("q.*k")))
    assertEquals(t1.indexOf("The"), l1.indexOf(Matcher.regexp("[Tt]he")))
    assertEquals(t1.lastIndexOf("the"), l1.lastIndexOf(Matcher.regexp("[Tt]he")))

    val t2 = "Em, tee, blah. Blah blah, teleblah."
    val l2 = Line(t2)
    assertTrue(l2.matches(Matcher.regexp("Em.*, blah.")))
    assertTrue(l2.matches(Matcher.regexp("Em.*, teleblah.")))
    assertFalse(l2.matches(Matcher.regexp("Em.*hello.*blah.")))
    assertEquals(9, l2.indexOf(Matcher.regexp("blah")))
    assertEquals(t2.lastIndexOf("blah"), l2.lastIndexOf(Matcher.regexp("blah")))
    assertEquals(t2.lastIndexOf("blah,"), l2.lastIndexOf(Matcher.regexp("\\bblah")))
    assertEquals(t2.lastIndexOf("blah.", 14), l2.lastIndexOf(Matcher.regexp("\\bblah"), 14))
  }
}
