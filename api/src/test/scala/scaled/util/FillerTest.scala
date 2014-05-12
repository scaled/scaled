//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import org.junit.Assert._
import org.junit._
import scaled._

class FillerTest {

  @Test def testFill () {
    val filler = new Filler(40)
    val line = Line("Now is the time for all good men to come to the aid of their country. ")
    filler.append(line)
    filler.append(line)
    // filler.result foreach println
    assertEquals(Seq(Line("Now is the time for all good men to come"),
                     Line("to the aid of their country. Now is the"),
                     Line("time for all good men to come to the aid"),
                     Line("of their country.")), filler.result)

    // append line twice more, which causes the text to just exceeds 40 columns in the last line
    // with a single trailing blank space; that should be ignored
    filler.append(line)
    filler.append(line)
    // filler.result foreach println
    assertEquals(7, filler.result.size)
  }

  @Test def testCompact () {
    val filler = new Filler(40)
    filler.append(Line("one   two   three   "))
    assertEquals(Seq(Line("one two three")), filler.result)
  }

  @Test def testWALLOFTEXT () {
    val filler = new Filler(40)
    // TODO: should we allow WALLOFTEXT to exceed fill column...?
    filler.append(Line("IDONTLIKEOTPUTSPACESINMYTEXTITMAKESMEFEELINSCURE but"))
    filler.append(Line("some people don't mind so much..."))
    // filler.result foreach println
    assertEquals(Seq(Line("IDONTLIKEOTPUTSPACESINMYTEXTITMAKESMEFEE"),
                     Line("LINSCURE but some people don't mind so"),
                     Line("much...")), filler.result)
  }

  @Test def testLineJustFits () {
    val lines = Seq(Line("This line just fits into forty columns."),
                    Line("Which means that we'll rebreak immediately"),
                    Line("appending the second line."))
    val filler = new Filler(40)
    lines foreach filler.append
    val filled = filler.result
    assertEquals(lines.head, filled.head)
  }

  @Test def testNotNipLastChar () {
    // this first line is exactly 40 characters long, and I was seeing a bug when the first line
    // was exactly the width of the filler it would clip the last character from the line, so this
    // test ensures that we don't regress
    val lines = Seq(Line("Now is the time for all good men to come"),
                    Line("to the aid of their country."))
    val filler = new Filler(40)
    lines foreach filler.append
    assertEquals(lines, filler.result)
  }
}
