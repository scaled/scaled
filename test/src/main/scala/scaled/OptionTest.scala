//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class OptionTest {

  @Test def testMap () {
    assertEquals(None, (None :Option[Int]).map(_ + 2))
    assertEquals(Some(5), Some(3).map(_ + 2))
  }

  @Test def testFlatMap () {
    assertEquals(None, (None :Option[Int]).flatMap(x => Some(x + 2)))
    assertEquals(Some(5), Some(2).flatMap(x => Some(x + 3)))
  }

  @Test def testPatternMatch {
    val opt = Some(5)
    opt match {
      case None => fail()
      case Some(x) => assertEquals(5, x)
    }

    val nopt :Option[Int] = None
    var matched = false
    nopt match {
      case None => matched = true
      case Some(x) => matched = false
    }
    assertTrue(matched)
  }
}
