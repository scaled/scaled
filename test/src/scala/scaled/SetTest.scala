//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.Arrays
import org.junit._
import org.junit.Assert._

class SetTest {

  @Test def testBasics () :Unit = {
    val set = Set("ant", "bob", "cat", "dog", "egg", "frog", "goat", "horse")
    assertEquals(8, set.size)
    Seq("ant", "bob", "cat", "dog", "egg", "frog", "goat", "horse") foreach { elem =>
      assertTrue(s"contains $elem", set(elem))
    }
    Seq("peanut", "popcorn", "jellybean", "licorice", "hotdog", "nachos") foreach { elem =>
      assertFalse(s"does not contain $elem", set(elem))
    }
  }

  @Test def testRepeats () :Unit = {
    val set = Set("ant", "bob", "ant", "cat", "dog", "bob", "egg", "bob")
    assertEquals(5, set.size)
    Seq("ant", "bob", "cat", "dog", "egg") foreach { elem =>
      assertTrue(s"contains $elem", set(elem))
    }
    Seq("peanut", "popcorn", "jellybean", "licorice", "hotdog", "nachos") foreach { elem =>
      assertFalse(s"does not contain $elem", set(elem))
    }
  }

  @Test def testAlgebra () :Unit = {
    val set0 = Set("a", "b", "c", "d")
    val set1 = Set("e", "f", "c", "d")
    assertEquals(Set("a", "b", "c", "d", "e", "f"), set0 | set1)
    assertEquals(Set("c", "d"), set0 & set1)
    assertEquals(Set("a", "b"), set0 &~ set1)
    assertEquals(Set("e", "f"), set1 &~ set0)

    assertEquals(Set.empty, set0 & Set.empty)
    assertEquals(set0, set0 | Set.empty)
    assertEquals(set0, set0 &~ Set.empty)
    assertEquals(Set.empty, Set.empty &~ set0)
  }

  @Test def testFlatMap () :Unit = {
    val set0 = Set("one", "two", "three")
    assertEquals(Set('o', 'n', 'e', 't', 'w', 'h', 'r'), set0.flatMap(s => Seq.from(s.toArray)))
  }

  @Test def testEmptyOHS () :Unit = {
    val set = Set.builder[String]().build()
    assertTrue(set.isInstanceOf[OpenHashSet[_]])
    assertFalse(set.contains("peanut"))
    assertEquals(0, set.size)
    assertFalse(set.iterator().hasNext)
    assertEquals(Set.empty, set)
    assertEquals(set, Set.empty)
  }

  @Test def testSortedSet () :Unit = {
    val els = Seq("one", "two", "three", "four", "five")
    val set = Set.builder[String]().append(els).buildSorted()
    assertEquals(Seq("five", "four", "one", "three", "two"), set.toSeq)

    val bylen = Set.builder[String]().append(els).buildSorted(
      implicitly[Ordering[Int]].on[String](_.length))
    assertEquals(Seq("one", "two", "four", "five", "three"), bylen.toSeq)
  }
}
