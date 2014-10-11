//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class SeqBufferTest {

  val s1to4 = Seq(1, 2, 3, 4)
  val s1to40 = Seq(1 to 40 :_*)

  @Test def testAppend () {
    val sb = SeqBuffer[Int]()
    1 to 4 foreach { sb += _ }
    assertEquals(s1to4, sb)
    assertEquals(sb, s1to4)
    sb.clear()

    sb ++= s1to4
    assertEquals(s1to4, sb)
    assertEquals(sb, s1to4)
    sb.clear()

    1 to 40 foreach { sb += _ }
    assertEquals(s1to40, sb)
    assertEquals(sb, s1to40)
    sb.clear()

    val sbs = SeqBuffer[String]()
    val strs = Seq("a", "b", "c", "d", "e")
    sbs ++= strs.iterator
    assertEquals(strs, sbs)
  }

  @Test def testInsert () {
    val sb = SeqBuffer[Int]()
    1 to 4 foreach { sb += _ }
    sb.insert(2, 3)
    assertEquals(Seq(1, 2, 3, 3, 4), sb)
    sb.clear()

    sb ++= s1to4
    sb.insert(2, s1to4)
    assertEquals(Seq(1, 2, 1, 2, 3, 4, 3, 4), sb)
    sb.clear()

    sb ++= s1to4
    sb.insert(2, s1to40)
    val expect = s1to4.take(2) ++ s1to40 ++ s1to4.drop(2)
    assertEquals(expect, sb)
    sb.clear()

    // 1 to 40 foreach { sb += _ }
    // assertEquals(s1to40, sb)
    // assertEquals(sb, s1to40)
    // sb.clear()
  }

  @Test def testRemove () {
    val sb = SeqBuffer[Int]()
    sb ++= s1to4
    sb.remove(2, 1)
    assertEquals(Seq(1, 2, 4), sb)

    sb.clear()
    sb ++= s1to4
    sb.remove(2, 2)
    assertEquals(Seq(1, 2), sb)

    sb.clear()
    sb ++= s1to4
    sb.remove(2, 0)
    assertEquals(s1to4, sb)

    sb.clear()
    sb ++= s1to4
    sb.remove(2)
    assertEquals(Seq(1, 3, 4), sb)

    sb.clear()
    sb ++= s1to40
    sb.remove(1, 38)
    assertEquals(Seq(1, 40), sb)

    var threw = false
    try {
      sb.clear()
      sb ++= s1to4
      sb.remove(-2, 1)
    } catch {
      case ioobe :IndexOutOfBoundsException => threw = true
    }
    assertTrue("remove(-2, 1) should have thrown IOOBE", threw)

    threw = false
    try {
      sb.clear()
      sb ++= s1to4
      sb.remove(2, 5)
    } catch {
      case ioobe :IndexOutOfBoundsException => threw = true
    }
    assertTrue("remove(2, 5) should have thrown IOOBE", threw)
  }
}
