//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit.Assert._
import org.junit._
import scala.collection.mutable.ArrayBuffer

class TagTest {

  @Test def testTagAt () {
    val tags = new Tags()
    tags.add("one", 1, 5)
    tags.add("two", 4, 8)
    tags.add(2, 4, 8) // should not match our type tests
    tags.add("three", 6, 8)

    assertEquals("one", tags.tagAt(classOf[String], 1, ""))
    assertEquals("one", tags.tagAt(classOf[String], 2, ""))
    assertEquals("one", tags.tagAt(classOf[String], 3, ""))
    assertEquals("one", tags.tagAt(classOf[String], 4, ""))
    assertEquals("two", tags.tagAt(classOf[String], 5, ""))
    assertEquals("two", tags.tagAt(classOf[String], 6, ""))
    assertEquals("two", tags.tagAt(classOf[String], 7, ""))
    assertEquals("", tags.tagAt(classOf[String], 8, ""))
  }

  @Test def testAddOrder () {
    val tags = new Tags()
    tags.add("two", 4, 8)
    tags.add("three", 6, 8)
    tags.add("one", 1, 5)
    tags.add(2, 4, 8) // should not match our type tests

    assertEquals("one", tags.tagAt(classOf[String], 1, ""))
    assertEquals("one", tags.tagAt(classOf[String], 2, ""))
    assertEquals("one", tags.tagAt(classOf[String], 3, ""))
    assertEquals("one", tags.tagAt(classOf[String], 4, ""))
    assertEquals("two", tags.tagAt(classOf[String], 5, ""))
    assertEquals("two", tags.tagAt(classOf[String], 6, ""))
    assertEquals("two", tags.tagAt(classOf[String], 7, ""))
    assertEquals("", tags.tagAt(classOf[String], 8, ""))
  }

  @Test def testTagsAt () {
    val tags = new Tags()
    tags.add("one", 1, 5)
    tags.add("two", 4, 8)
    tags.add(2, 4, 8) // should not match our type tests
    tags.add("three", 6, 8)

    assertEquals(List("one"),          tags.tagsAt(classOf[String], 1))
    assertEquals(List("one"),          tags.tagsAt(classOf[String], 2))
    assertEquals(List("one"),          tags.tagsAt(classOf[String], 3))
    assertEquals(List("two", "one"),   tags.tagsAt(classOf[String], 4))
    assertEquals(List("two"),          tags.tagsAt(classOf[String], 5))
    assertEquals(List("three", "two"), tags.tagsAt(classOf[String], 6))
    assertEquals(List("three", "two"), tags.tagsAt(classOf[String], 7))
    assertEquals(Nil, tags.tagsAt(classOf[String], 8))
  }

  @Test def testSimpleVisit () {
    val tags = new Tags()
    tags.add("a", 0, 4)
    tags.add("b", 10, 15)
    tags.add("c", 20, 22)
    testVisit(tags, Seq(( 0,  4, Seq("a")),
                        (10, 15, Seq("b")),
                        (20, 22, Seq("c"))))
  }

  @Test def testVisitOverlaps () {
    val tags = new Tags()
    tags.add("a", 0, 4)
    tags.add("b", 1, 5)
    tags.add("c", 2, 6)
    tags.add("d", 3, 7)
    tags.add("e", 4, 8)
    testVisit(tags, Seq((0, 1, Seq("a")),
                        (1, 2, Seq("a", "b")),
                        (2, 3, Seq("a", "b", "c")),
                        (3, 4, Seq("a", "b", "c", "d")),
                        (4, 5, Seq("b", "c", "d", "e")),
                        (5, 6, Seq("c", "d", "e")),
                        (6, 7, Seq("d", "e")),
                        (7, 8, Seq("e"))))
  }

  @Test def testVisitEnds () {
    val tags = new Tags()
    tags.add("a", 0, 4)
    tags.add("b", 0, 3)
    tags.add("c", 0, 2)
    tags.add("d", 0, 1)
    tags.add("mid", 7, 9)
    tags.add("A", 10, 14)
    tags.add("B", 10, 13)
    tags.add("C", 10, 12)
    tags.add("D", 10, 11)
    testVisit(tags, Seq(( 0,  1, Seq("a", "b", "c", "d")),
                        ( 1,  2, Seq("a", "b", "c")),
                        ( 2,  3, Seq("a", "b")),
                        ( 3,  4, Seq("a")),
                        ( 7,  9, Seq("mid")),
                        (10, 11, Seq("A", "B", "C", "D")),
                        (11, 12, Seq("A", "B", "C")),
                        (12, 13, Seq("A", "B")),
                        (13, 14, Seq("A"))))
  }

  @Test def testRemove () {
    val tags = new Tags()
    tags.add("left", 2, 6)
    assertTrue(tags.remove("left", 0, 4))
    tags.add("right", 10, 14)
    assertTrue(tags.remove("right", 12, 18))
    tags.add("inner", 20, 30)
    assertTrue(tags.remove("inner", 22, 28))
    tags.add("outer", 30, 35)
    assertTrue(tags.remove("outer", 28, 36))
    tags.add("exact", 10, 20)
    assertTrue(tags.remove("exact", 10, 20))

    testVisit(tags, Seq((4, 6, Seq("left")),
                        (10, 12, Seq("right")),
                        (20, 22, Seq("inner")),
                        (28, 30, Seq("inner")))) // outer and exact fully removed

  }

  @Test def testSlice () {
    val tags = new Tags()
    tags.add("R", 0, 5)
    tags.add("A", 3, 6)
    tags.add("E", 2, 8)
    tags.add("L", 6, 10)
    tags.add("C", 0, 12)

    // 01|234567|89012
    // RR|RRR   |
    //   | AAA  |
    //   |EEEEEE|
    //   |    LL|LL
    // CC|CCCCCC|CCCCC
    //    012345

    val slice = tags.slice(2, 8)
    assertEquals(List((0, 3, "R"), (0, 6, "C"), (0, 6, "E"), (1, 4, "A"), (4, 6, "L")),
                 slice.tags.map(t => (t.start, t.end, t.tag.toString)))
  }

  @Test def testDelete () {
    val tags = new Tags()
    tags.add("R", 0, 5)
    tags.add("A", 3, 6)
    tags.add("E", 2, 8)
    tags.add("L", 6, 10)
    tags.add("C", 0, 12)

    // 01|234567|8901
    // RR|RRR   |
    //   | AAA  |
    //   |EEEEEE|
    //   |    LL|LL
    // CC|CCCCCC|CCCC
    // 01        2345

    tags.delete(2, 8)
    assertEquals(List((0, 2, "R"), (0, 2, "C"), (2, 6, "C"), (2, 4, "L")),
                 tags.tags.map(t => (t.start, t.end, t.tag.toString)))
  }

  @Test def testClear () {
    val tags = new Tags()
    tags.add("R", 0, 5)
    tags.add("A", 3, 6)
    tags.add("E", 2, 8)
    tags.add("L", 6, 10)
    tags.add("C", 0, 12)

    // 01|234567|8901
    // RR|RRR   |
    //   | AAA  |
    //   |EEEEEE|
    //   |    LL|LL
    // CC|CCCCCC|CCCC
    // 01        8901

    tags.clear(2, 8)
    assertEquals(List((0, 2, "R"), (0, 2, "C"), (8, 12, "C"), (8, 10, "L")),
                 tags.tags.map(t => (t.start, t.end, t.tag.toString)))
  }

  @Test def testExpand () {
    val tags = new Tags()
    tags.add("O", 0, 6)
    tags.add("L", 0, 2)
    tags.add("R", 4, 8)

    // 0123|4567     0123|45|6789
    // OOOO|OO    => OOOO|OO|OO
    // LL  |      => LL  |  |
    //     |RRRR            |RRRR

    tags.expand(4, 2)
    assertEquals(List((0, 8, "O"), (0, 2, "L"), (6, 10, "R")),
                 tags.tags.map(t => (t.start, t.end, t.tag.toString)))
  }

  private def testVisit (tags :Tags, expect :Seq[(Int,Int,Seq[String])]) {
    val groups = ArrayBuffer[(Int,Int,Seq[String])]()
    tags.visit(classOf[String])((ts, start, end) => groups += ((start, end, ts.map(_.tag))))
    assertEquals(expect.size, groups.size)
    for ((exp,got) <- expect zip groups) assertEquals(exp, got)
  }
}
