//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class StylesTest {

  @Test def testStyles () {
    val s1 = Styles("one")
    assertEquals("one", s1.toString)
    assertTrue((s1 + "one") eq s1)
    assertTrue(s1 contains "one")
    assertFalse(s1 contains "two")

    val s2 = Styles("two")
    assertEquals("two", s2.toString)
    assertTrue((s2 + "two") eq s2)
    assertFalse(s2 contains "one")
    assertTrue(s2 contains "two")

    val s12 = s1 + "two"
    assertEquals("one two", s12.toString)
    val s21 = s2 + "one"
    assertEquals("one two", s21.toString)
    assertTrue(s12 eq s21)
    assertTrue(s12 contains "one")
    assertTrue(s12 contains "two")
    assertFalse(s12 contains "three")

    assertTrue(s12 - "one" eq s2)
    assertTrue(s12 - "two" eq s1)

    val s3 = Styles("one") + "two" + "three" + "four" + "five"
    val s3minusTs = Styles("one") + "four" + "five"
    assertEquals(s3minusTs, s3 - (_ startsWith "t"))
  }
}
