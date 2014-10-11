//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.Arrays
import org.junit._
import org.junit.Assert._

class ListTest extends OrderedTestBase {

  @Test def testBuilder () {
    val lb = List.builder[String]
    lb += "one"
    lb += "two"
    lb += "three"
    assertEquals("one" :: "two" :: "three" :: Nil, lb.build())

    // make sure we convert from a Scala Seq properly
    val lb2 = List.builder[String]
    lb2 ++= scala.Seq("one", "two", "three")
    assertEquals(List("one", "two", "three"), lb2.build())
  }

  @Test def testReverse () {
    assertEquals(Nil, Nil.reverse)
    assertEquals(List("a"), List("a").reverse)
    assertEquals(List("c", "b", "a"), List("a", "b", "c").reverse)
    assertEquals(List("d", "c", "b", "a"), List("a", "b", "c", "d").reverse)
  }

  @Test def testConstravariance () {
    val ints :List[Integer] = 1 :: Nil
    val nums :List[Number] = 3f :: ints
    assertEquals(1, ints.size)
    assertEquals(2, nums.size)
  }

  @Test def testCopyInto () {
    val strs = List("a", "b", "c")
    val a0 = new Array[Any](3)
    strs.copyInto(0, strs.length, a0, 0)
    assertEquals(Seq("a", "b", "c"), a0.asInstanceOf[Array[AnyRef]].mkSeq)

    val a1 = new Array[Any](5)
    strs.copyInto(0, strs.length, a1, 1)
    assertEquals(Seq(null, "a", "b", "c", null), a1.asInstanceOf[Array[AnyRef]].mkSeq)
  }

  @Test def testCovariance () {
    def length (os :List[Any]) :Int = os.length
    assertEquals(5, length(List(1, 2, 3, 4, 5)))
  }

  @Test def testFolds () {
    assertEquals("abc", ("" /: List("a", "b", "c"))(_ + _))
    assertEquals("cba", (List("a", "b", "c") :\ "")((a,b) => b+a))
  }

  @Test def testFrom () {
    assertEquals(List(1, 2, 3), List.from(Array(1, 2, 3)))
  }

  @Test def testFromScala () {
    val slist = "a" :: "b" :: "c" :: SNil
    assertEquals(List("a", "b", "c"), slist.fromScala)
  }

  @Test def testPatternMatch () {
    val as = List("one", "two", "three")
    as match {
      case h :: t =>
        assertEquals("one", h)
        assertEquals(as.tail, t)
      case _ => fail()
    }
    as match {
      case h1 :: h2 :: t =>
        assertEquals("one", h1)
        assertEquals("two", h2)
        assertEquals(List("three"), t)
      case _ => fail()
    }
    as match {
      case h1 :: h2 :: h3 :: Nil =>
        assertEquals("one", h1)
        assertEquals("two", h2)
        assertEquals("three", h3)
      case _ => fail()
    }

    var matched = false
    List.nil[String] match {
      case h :: t => // bad
      case _ => matched = true
    }
    assertTrue(matched)
  }

  protected def empty[A] = Nil
  protected def builder[A] (esize :Int) = List.builder()
}
