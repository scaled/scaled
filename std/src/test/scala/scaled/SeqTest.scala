//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.Arrays
import org.junit._
import org.junit.Assert._

class SeqTest extends OrderedTestBase {

  @Test def testApply () {
    val is = Seq(1, 2, 3, 4)
    assertEquals(1, is(0))
    assertEquals(2, is(1))
    assertEquals(3, is(2))
    assertEquals(4, is(3))
  }

  @Test def testBoxedPrimitives () {
    val ints = Array(1, 2, 3, 4)
    val iseq = Seq.from(ints)
    assertEquals(Seq(1, 2, 3, 4), iseq)
  }

  @Test def testContains () {
    val is = Seq(1, 2, 3, 4)
    assertTrue(is contains 2)
    assertFalse(is contains 5)

    val as = Seq("one", "two", "three")
    assertTrue(as contains "one")
    assertFalse(as contains "four")

    assertFalse(Seq[String]() contains "bob")
  }

  @Test def testCount () {
    val as = Seq(1, 2, 3, 4)
    assertEquals(2, as.count(_ > 2))
    assertEquals(1, as.count(_ < 2))

    assertEquals(0, Seq[String]().count(_ == "foo"))
  }

  @Test def testCovariance () {
    def length (os :Seq[Any]) :Int = os.length
    assertEquals(5, length(Seq(1, 2, 3, 4, 5)))
  }

  @Test def testDistinct () {
    val as = Seq(1, 2, 3, 2, 4, 1, 3, 5)
    assertEquals(Seq(1, 2, 3, 4, 5), as.distinct)
  }

  @Test def testFlatMap () {
    val strs = Seq("one", "two", "three", "four")
    def isEven (str :String) = if (str.length % 2 == 0) Some(str) else None
    assertEquals(Seq("four"), strs.flatMap(s => isEven(s)))
  }

  @Test def testFolds () {
    assertEquals("abc", ("" /: Seq("a", "b", "c"))(_ + _))
    assertEquals("cba", (Seq("a", "b", "c") :\ "")((a,b) => b+a))
  }

  @Test def testFromScala () {
    val sseq = scala.Seq("a", "b", "c")
    assertEquals(Seq("a", "b", "c"), sseq.fromScala)
  }

  @Test def testIndexOf () {
    val strs = Seq("a", "b", "c", "b", "d")
    assertEquals(1, strs.indexOf("b"))
    assertEquals(3, strs.lastIndexOf("b"))
    assertEquals(-1, strs.indexOf("z"))
  }

  @Test def testIterToSeq () {
    val list = Arrays.asList("a", "b", "c", "d")
    assertEquals(Seq("a", "b", "c", "d"), list.iterator.toSeq)
  }

  @Test def testUnapply () {
    val one = Seq("one")
    one match {
      case Seq(a) => assertEquals("one", a)
      case _ => fail()
    }
    one match {
      case Seq("one") => // yay
      case _ => fail()
    }
    one match {
      case Seq("two") => fail()
      case _ => // yay!
    }
  }

  @Test def testUnapplySeq () {
    val as = Seq("one", "two", "three")
    as match {
      case Seq(h, _*) => assertEquals("one", h)
      case _ => fail()
    }
    as match {
      case Seq(a, b, c) =>
        assertEquals("one", a)
        assertEquals("two", b)
        assertEquals("three", c)
      case _ => fail()
    }
    as match {
      case Seq("one", b, c) =>
        assertEquals("two", b)
        assertEquals("three", c)
      case _ => fail()
    }
    as match {
      case Seq("two", b, c) => fail()
      case _ => // yay!
    }
    as match {
      case Seq(a, b, c, d) => fail()
      case _ => // yay!
    }
  }

  @Test def testReverse () {
    assertEquals(Seq.empty, Seq.empty.reverse)
    assertEquals(Seq("a"), Seq("a").reverse)
    assertEquals(Seq("c", "b", "a"), Seq("a", "b", "c").reverse)
    assertEquals(Seq("d", "c", "b", "a"), Seq("a", "b", "c", "d").reverse)
  }

  @Test def testSlices () {
    val full = Seq("a", "b", "c", "d", "e", "f")
    val bcd = Seq("b", "c", "d")
    val bce = Seq("b", "c", "e")
    assertTrue(full.containsSlice(bcd))
    assertFalse(full.containsSlice(bce))
    assertEquals(1, full.indexOfSlice(bcd))
    assertEquals(-1, full.indexOfSlice(bce))

    val moar = Seq("a", "b", "c", "d", "e", "f", "b", "c", "d", "e")
    assertEquals(1, moar.indexOfSlice(bcd))
    assertEquals(6, moar.lastIndexOfSlice(bcd))
    assertEquals(-1, moar.indexOfSlice(bce))
    assertEquals(-1, moar.lastIndexOfSlice(bce))
  }

  protected def empty[A] = Seq.empty
  protected def builder[A] (esize :Int) = Seq.builder()
}
