//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.Arrays
import org.junit._
import org.junit.Assert._

abstract class OrderedTestBase {

  @Test def testCollect () {
    val strs = make("one", "two", "three", "four")
    assertEquals(Seq(3, 3, 5), strs collect { case str if (str.length % 2 == 1) => str.length })
  }

  @Test def testCollectFirst () {
    val strs = make("one", "two", "three", "four")
    assertEquals(Some(4), strs collectFirst { case str if (str.length % 2 == 0) => str.length })
    assertEquals(None, strs collectFirst { case str if (str.length > 10) => str.length })
  }

  @Test def testDrop () {
    val one23 = make(1, 2, 3)
    val one2345 = make(1, 2, 3, 4, 5)
    assertEquals(make(3, 4, 5), one2345.drop(2))
    assertEquals(empty, one23.drop(Int.MaxValue))
    assertEquals(empty, one23.drop(4))
    assertEquals(one23, one23.drop(0))
    assertEquals(one23, one23.drop(-4))
    assertEquals(one23, one23.drop(Int.MinValue))

    assertEquals(one23, one2345.dropRight(2))
    assertEquals(empty, one23.dropRight(Int.MaxValue))
    assertEquals(empty, one23.dropRight(4))
    assertEquals(one23, one23.dropRight(0))
    assertEquals(one23, one23.dropRight(-4))
    assertEquals(one23, one23.dropRight(Int.MinValue))

    assertEquals(make(3, 4, 5), one2345.dropWhile(_ < 3))
    assertEquals(empty, one23.dropWhile(_ < 4))
  }

  @Test def testFind () {
    val strs = make("one", "two", "three", "four")
    assertEquals(None, strs.find(_.length == 2))
    assertEquals(Some("one"), strs.find(_.length == 3))
    assertEquals(Some("four"), strs.find(_.length == 4))
    assertEquals(Some("three"), strs.find(_.length == 5))
    assertEquals(None, strs.find(_.length == 2))
  }

  @Test def testEquals () {
    val c1 = make("one", "two", "three")
    val c2 = make("one", "two", "three")
    val c3 = make("one", "two", "three", "four")
    val c4 = make(1, 2, 3)
    assertEquals(c1, c2)
    assertEquals(c2, c1)
    assertNotEquals(c1, c3)
    assertNotEquals(c3, c1)
    assertNotEquals(c1, c4)
    assertNotEquals(c4, c1)
    assertEquals(make(), empty)
  }

  @Test def testEndsWith () {
    val c1 = make(1, 2, 3, 4, 5)
    val c2 = make(3, 4, 5)
    val c3 = make(0, 1, 2, 3, 4, 5)
    val c4 = make(5, 4, 3)
    assertTrue(c1 endsWith c1)
    assertTrue(c1 endsWith c2)
    assertTrue(c1 endsWith empty)
    assertFalse(c1 endsWith c3)
    assertFalse(c1 endsWith c4)
  }

  @Test def testExists () {
    val c1 = make(1, 2, 3, 4, 5)
    assertTrue(c1.exists(_ == 3))
    assertFalse(c1.exists(_ == 9))
  }

  @Test def testFilter () {
    val c1 = make(1, 2, 3, 4, 5)
    assertEquals(make(2, 4), c1.filter(_ %2 == 0))
    assertEquals(make(1, 3, 5), c1.filterNot(_ %2 == 0))
  }

  @Test def testForComp () {
    val ints = make(1, 2, 3, 4, 5, 6, 7)
    val evens = for (ii <- ints if (ii % 2 == 0)) yield ii
    assertEquals(make(2, 4, 6), evens)
    val evenStrs = for (ii <- ints if (ii % 2 == 0) ; jj = ii.toString) yield jj
    assertEquals(make("2", "4", "6"), evenStrs)

    val strs = make("one", "two", "three", "four")
    val evenLens = for (ss <- strs ; ll = ss.length ; if (ss.length % 2 == 0)) yield ss
    assertEquals(make("four"), evenLens)

    val earlyChars = for (ss <- strs ; rs = ss.reverse ;
                          cc <- Seq(rs.toArray :_*) if (cc < 'f')) yield cc
    assertEquals(make('e', 'e', 'e'), earlyChars)

    var odds = 0
    for (s <- strs if (s.length % 2 == 1)) odds += 1
    assertEquals(3, odds)
  }

  @Test def testGrouped () {
    val as = make(1, 2, 3, 4, 5, 6, 7)
    assertEquals(make(make(1, 2, 3), make(4, 5, 6), make(7)), as.grouped(3))
  }

  @Test def testGroupBy () {
    val strs = make("one", "two", "three", "four", "five")
    assertEquals(Map(3 -> make("one", "two"),
                     4 -> make("four", "five"),
                     5 -> make("three")), strs.groupBy(_.length))
  }

  @Test def testHashCode () {
    // hash code should be equivalent to Arrays.hashCode
    val ints = make(1, 2, 3, 4, 5)
    assertEquals(ints.hashCode, Arrays.hashCode(Array(1, 2, 3, 4, 5)))
  }

  @Test def testLast () {
    assertEquals(5, make(1, 2, 3, 4, 5).last)
    assertEquals(2, make(1, 2).last)
  }

  @Test(expected=classOf[NoSuchElementException]) def testLastOnEmpty () {
    assertEquals(5, empty.last)
  }

  @Test def testMap () {
    assertEquals(make(3, 3, 5, 4), make("one", "two", "three", "four").map(_.length))
  }

  @Test def testMinMax () {
    val is = make(9, 12, 5, 27, 3)
    assertEquals(27, is.max)
    assertEquals( 3, is.min)
    val ss = make("a", "z", "b")
    assertEquals("a", ss.min)
    assertEquals("z", ss.max)
    val strs = make("one", "two", "three", "four", "z")
    assertEquals("three", strs.maxBy(_.length))
    assertEquals("z", strs.minBy(_.length))
  }

  @Test def testPartition () {
    val is = make(5, 3, 7, 4, 12, 9, 2)
    assertEquals((make(5, 3, 7, 9), make(4, 12, 2)), is.partition(_ % 2 != 0))
  }

  @Test def testReduce () {
    val is = make(1, 2, 3, 4)
    assertEquals(10, is.reduceLeft(_ + _))
    def plus (a :Int, b :Int) = a + b
    assertEquals(10, is reduceLeft plus)
  }

  @Test def testSize () {
    assertEquals(0, empty.size)
    val ints = make(1, 2, 3, 4, 5)
    assertEquals(5, ints.size)
    assertEquals(4, ints.drop(1).size)
  }

  @Test def testTake () {
    val one23 = make(1, 2, 3)
    val one2345 = make(1, 2, 3, 4, 5)

    assertEquals(one2345, one2345.take(Int.MaxValue))
    assertEquals(one2345, one2345.take(6))
    assertEquals(one23, one2345.take(3))
    assertEquals(empty, one23.take(0))
    assertEquals(empty, one23.take(-4))
    assertEquals(empty, one23.take(Int.MinValue))

    assertEquals(one2345, one2345.takeRight(Int.MaxValue))
    assertEquals(one2345, one2345.takeRight(6))
    assertEquals(make(3, 4, 5), one2345.takeRight(3))
    assertEquals(empty, one2345.takeRight(0))
    assertEquals(empty, one2345.takeRight(-4))
    assertEquals(empty, one2345.takeRight(Int.MinValue))

    assertEquals(one23, one2345.takeWhile(_ < 4))
    assertEquals(empty, one2345.takeWhile(_ > 10))
  }

  @Test def testToArray () {
    val as = make("a", "b", "c")
    assertTrue(Arrays.deepEquals(Array("a", "b", "c") :Array[Object], as.toArray :Array[Object]))
  }

  @Test def testZip () {
    val as = make("a", "b", "c")
    val bs = make(1, 2, 3, 4, 5)

    assertEquals(make(("a", 1), ("b", 2), ("c", 3)), as zip bs)
    assertEquals(make((1, "a"), (2, "b"), (3, "c")), bs zip as)

    assertEquals(make(("a", 0), ("b", 1), ("c", 2)), as.zipWithIndex)
    assertEquals(make(("a", 1), ("b", 2), ("c", 3), ("z", 4), ("z", 5)), as zipAll(bs, "z", 99))
    assertEquals(make((1, "a"), (2, "b"), (3, "c"), (4, "z"), (5, "z")), bs zipAll(as, 99, "z"))
  }

  protected def empty[A] :Ordered[A]
  protected def builder[A] (esize :Int) :Ordered.Builder[A]

  protected def make[A] () :Ordered[A] = builder(0).build()
  protected def make[A] (e0 :A) :Ordered[A] = builder(1).append(e0).build()
  protected def make[A] (e0 :A, e1 :A) :Ordered[A] = builder(2).append(e0).append(e1).build()
  protected def make[A] (e0 :A, e1 :A, e2 :A) :Ordered[A] =
    builder(3).append(e0).append(e1).append(e2).build()
  protected def make[A] (e0 :A, e1 :A, e2 :A, rest :A*) :Ordered[A] =
    builder(3+rest.size).append(e0).append(e1).append(e2).append(rest).build()
}
