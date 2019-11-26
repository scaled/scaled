//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit.Assert._
import org.junit._

class SearchTest {

  val WHO = "Who was that man?"
  val NOW = "Now is the time for all good citizens to come to the aid of their country."
  val EGBDF = "Every good boy deserves fudge."
  val ABC = "The quick brown fox jumped over the lazy dog."

  val text = Seq(WHO, NOW, EGBDF, ABC)
  val buffer = Buffer("test", text.map(Line.apply))

  @Test def testSingleLineSearch () :Unit = {
    val the = Line("the")
    // search for all the thes in the whole buffer
    val allThes = Search(buffer, buffer.start, buffer.end, the).findAll()
    assertEquals(5, allThes.size)
    // search for thes on the first line (should be none)
    val l2 = Loc(1, 0)
    val firstThes = Search(buffer, buffer.start, l2, the).findAll()
    assertEquals(0, firstThes.size)
    // search for thes in the first half of the second line (should be one)
    val l2half = Loc(1, 20)
    val firstHalfThes = Search(buffer, l2, l2half, the).findAll()
    assertEquals(1, firstHalfThes.size)
    // search for thes in the second line just before the second 'the' (should be one)
    val l2most = Loc(1, 51)
    // println(buffer.region(l2, l2most).map(_.asString).mkString)
    val firstMostThes = Search(buffer, l2, l2most, the).findAll()
    assertEquals(1, firstMostThes.size)
    // search for thes in the second line just including the second 'the' (should be two)
    val l2mostp = Loc(1, 52)
    // println(buffer.region(l2, l2mostp).map(_.asString).mkString)
    val firstMostPThes = Search(buffer, l2, l2mostp, the).findAll()
    assertEquals(2, firstMostPThes.size)
    // search for thes on the whole second line (should be three)
    val l3 = Loc(2, 0)
    val secondThes = Search(buffer, l2, l3, the).findAll()
    assertEquals(3, secondThes.size)
    // search for at most two thes on the whole second line (should be two)
    val secondLimitThes = Search(buffer, l2, l3, the).findAll().take(2)
    assertEquals(2, secondLimitThes.size)
  }

  // TODO: @Test def testMultiLineSearch () :Unit = {}
}
