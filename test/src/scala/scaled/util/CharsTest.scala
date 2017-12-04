//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import org.junit.Assert._
import org.junit._

class CharsTest {

  @Test def testPreds () {
    test(true, Chars.isWhitespace, ' ')
    test(false, Chars.isNotWhitespace, ' ')
    test(true, Chars.isNotWhitespace, 'b')

    test(true, Chars.isWord, 'b')
    test(false, Chars.isNotWord, 'b')
    test(true, Chars.isNotWord, ' ')

    test(true, Chars.isPunctuation, '\'')
    test(false, Chars.isNotPunctuation, '\'')
    test(true, Chars.isNotPunctuation, ' ')
  }

  private def test (expect :Boolean, fn :(Char => Boolean), c :Char) {
    assertEquals(s"$fn('$c')", expect, fn(c))
  }
}
