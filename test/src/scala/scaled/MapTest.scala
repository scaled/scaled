//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class MapTest {

  @Test def testEquals () :Unit = {
    assertEquals(Map("a" -> 1, "b" -> 2),
                 Map("b" -> 2, "a" -> 1))
    assertNotEquals(Map("a" -> 1, "b" -> 2),
                    Map("b" -> 2, "a" -> 1, "c" -> 3))
  }

  @Test def testDuplicatesInBuilder () :Unit = {
    val mb = Map.builder[String,Int]()
    mb += ("a", 1)
    mb += ("b", 2)
    mb += ("a", 3)
    assertEquals(Map("a" -> 3, "b" -> 2), mb.build())
  }

  @Test def testKeySet () :Unit = {
    val map = Map("a" -> 1, "b" -> 2)
    assertEquals(Set("a", "b"), map.keySet)
  }

  @Test def testContainsGet () :Unit = {
    val map = Map("a" -> 1, "b" -> 2)
    assertTrue(map.contains("a"))
    assertTrue(map.contains("b"))
    assertFalse(map.contains("c"))

    assertEquals(1, map("a"))
    assertEquals(Some(2), map.get("b"))
    assertEquals(None, map.get("c"))
  }

  @Test def testEmptyOHM () :Unit = {
    val map = Map.builder[String,String]().build()
    assertTrue(map.isInstanceOf[OpenHashMap[_,_]])
    assertFalse(map.contains("peanut"))
    assertEquals(None, map.get("peanut"))
    assertEquals(0, map.size)
    assertFalse(map.iterator().hasNext)
    assertEquals(Map.empty, map)
    assertEquals(map, Map.empty)
  }

  @Test def testCollect () :Unit = {
    val evens = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4) collect {
      case (k, v) if (v % 2 == 0) => v
    }
    assertEquals(Seq(2, 4), evens)
  }
}
