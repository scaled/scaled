//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class ScalaListTest {

  @Test def testSize () {
    assertEquals(0, Nil.size)
    val ints = Data.list(1, 2, 3, 4, 5)
    assertEquals(5, ints.size)
    assertEquals(4, ints.tail.size)
    val more = 6 :: ints
    assertEquals(6, more.size)
  }

  @Test def testConstravariance () {
    val ints :List[Integer] = 1 :: Nil
    val nums :List[Number] = 3f :: ints
    assertEquals(1, ints.size)
    assertEquals(2, nums.size)
  }
}
