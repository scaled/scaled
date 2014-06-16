//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import org.junit.Assert._
import org.junit._
import scaled._

class FillerTest {

  def pad (lines :Seq[String]) :Seq[String] = {
    val max = lines.maxBy(_.length).length
    lines.map(l => s"%-${max}s".format(l))
  }

  def checkResult (lines :Seq[Line], filler :Filler) {
    val filled = filler.toLines
    if (lines != filled) {
      val exp = pad("* Expected *" +: lines.map(_.toString)) ; val epad = " " * exp(0).length
      val got = pad("* Got *" +: filled.map(_.toString))     ; val gpad = " " * got(0).length
      def fmt (p :(String,String)) = s"${p._1} ${p._2}"
      fail("Filler failure.\n" + exp.zipAll(got, epad, gpad).map(fmt).mkString("\n"))
    }
  }

  @Test def testFill () {
    val filler = new Filler(40)
    val line = Line("Now is the time for all good men to come to the aid of their country. ")
    filler.append(line)
    filler.append(line)
    // filler.toLines foreach println
    checkResult(Seq(Line("Now is the time for all good men to come"),
                    Line("to the aid of their country. Now is the"),
                    Line("time for all good men to come to the aid"),
                    Line("of their country.")), filler)

    // append line twice more, which causes the text to just exceeds 40 columns in the last line
    // with a single trailing blank space; that should be ignored
    filler.append(line)
    filler.append(line)
    // filler.toLines foreach println
    assertEquals(7, filler.toLines.size)
  }

  @Test def testCompact () {
    val filler = new Filler(40)
    filler.append(Line("one   two   three   "))
    checkResult(Seq(Line("one two three")), filler)
  }

  @Test def testWALLOFTEXT () {
    val filler = new Filler(40)
    // TODO: should we allow WALLOFTEXT to exceed fill column...?
    filler.append(Line("IDONTLIKEOTPUTSPACESINMYTEXTITMAKESMEFEELINSCURE but"))
    filler.append(Line("some people don't mind so much..."))
    // filler.toLines foreach println
    checkResult(Seq(Line("IDONTLIKEOTPUTSPACESINMYTEXTITMAKESMEFEE"),
                    Line("LINSCURE but some people don't mind so"),
                    Line("much...")), filler)
  }

  @Test def testLineJustFits () {
    val lines = Seq(Line("This line just fits into forty columns."),
                    Line("Which means that we'll rebreak immediately"),
                    Line("appending the second line."))
    val filler = new Filler(40)
    lines foreach filler.append
    val filled = filler.toLines
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
    checkResult(lines, filler)
  }

  @Test def testLastSpacePushesOverWidth () {
    // the first line fills up while we're in "wantsSpace" mode, which causes a bug where we see
    // the 'a' after the space and add a space and the 'a' and end up blowing past our width
    val lines = Seq(Line("Blah blah blah blah blah blah blah blah a"),
                    Line("Oh noes, we've blown it."))
    val filler = new Filler(40)
    lines foreach filler.append
    // filler.filled foreach println
    assertEquals(Line("Blah blah blah blah blah blah blah blah"), filler.toLines(0))
  }
}
