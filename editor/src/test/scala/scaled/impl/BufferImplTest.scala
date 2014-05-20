//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit.Assert._
import org.junit._
import scaled._

object BufferImplTest {

  val WHO = "Who was that man?"
  val NOW = "Now is the time for all good citizens to come to the aid of their country."
  val EGBDF = "Every good boy deserves fudge."
  val ABC = "The quick brown fox jumped over the lazy dog."
  val testText = s"$WHO\n$NOW\n$EGBDF\n$ABC\n"

  def testBuffer (text :String) = BufferImpl(new TextStore("test", "", text))
}

class BufferImplTest {
  import BufferImplTest._

  @Test def testBasics () {
    val buffer = testBuffer(testText)
    assertEquals(5, buffer.lines.size)
  }

  @Test def testLoc () {
    val buffer = testBuffer(testText)
    assertEquals(Loc(0, 5), buffer.loc(5))
    assertEquals(5, buffer.offset(Loc(0, 5)))
    assertEquals(Loc(1, 0), buffer.loc(18))
    assertEquals(18, buffer.offset(Loc(1, 0)))
    assertEquals(Loc(1, 4), buffer.loc(22))
    assertEquals(22, buffer.offset(Loc(1, 4)))
    // any offset beyond the end of the buffer should resolve to the last valid buffer pos
    assertEquals(Loc(4, 0), buffer.loc(testText.length))
    assertEquals(Loc(4, 0), buffer.loc(testText.length+20))
  }

  @Test def testMutate () {
    val buffer = testBuffer(testText)
    buffer.delete(Loc(1, 0), Loc(2, 0))
    assertEquals(EGBDF, buffer.line(1).asString)
    val sp = Loc(1, "Every good".length)
    buffer.split(sp)
    // TODO: ensure that the proper events are emitted?
    assertEquals("Every good", buffer.line(1).asString)
    assertEquals(" boy deserves fudge.", buffer.line(2).asString)
    buffer.insert(buffer.lineEnd(sp), Line(" smelling"))
    buffer.delete(buffer.lineEnd(sp), buffer.forward(buffer.lineEnd(sp), 1))
    assertEquals("Every good smelling boy deserves fudge.", buffer.line(1).asString)
  }

  @Test def testMotion () {
    val buffer = testBuffer(testText)
    val (start, end) = (buffer.start, buffer.end)
    val length = buffer.offset(end)
    for (off <- 0 to length) {
      val loc = buffer.loc(off)
      assertEquals(s"$start + $off = $loc", loc, buffer.forward(start, off))
      assertEquals(s"$loc - $off = $start", start, buffer.backward(loc, off))

      val bloc = buffer.loc(length-off)
      assertEquals(s"$end - $off = $bloc", bloc, buffer.backward(end, off))
      assertEquals(s"$bloc + $off = $end", end, buffer.forward(bloc, off))
    }
    // check forward past end of buffer and back past start
    assertEquals(end, buffer.forward(start, length+10))
    assertEquals(start, buffer.backward(end, length+10))
  }

  @Test def testRegion () {
    val buffer = testBuffer(testText)

    // slice out some regions and make sure they look sane
    val r0 = buffer.region(Loc(0, 0), Loc(1, 0))
    val r1 = buffer.region(Loc(0, 0), Loc(0, 3))
    val r2 = buffer.region(Loc(0, 16), Loc(1, 6))
    val r3 = buffer.region(Loc(0, 16), Loc(2, 5))

    def checkregions () {
      assertEquals(2, r0.length)
      assertEquals(WHO, r0(0).asString)
      assertEquals(0, r0(1).length)
      assertEquals(1, r1.length)
      assertEquals(WHO.substring(0, 3), r1(0).asString)
      assertEquals(2, r2.length)
      assertEquals(Seq(WHO.substring(16), NOW.substring(0, 6)), r2.map(_.asString))
      assertEquals(3, r3.length)
      assertEquals(Seq(WHO.substring(16), NOW, EGBDF.substring(0, 5)), r3.map(_.asString))
    }
    checkregions()

    // mutate the buffer by inserting regions and make sure that looks reasonable
    val i0size = buffer.lines.size + r0.size - 1
    buffer.insert(Loc(1, 0), r0)
    assertEquals(i0size, buffer.lines.size)
    // we should now have two copies of the first line
    assertEquals(Seq(WHO, WHO, NOW), buffer.lines.slice(0, 3).map(_.asString))

    // try inserting a single line region
    val rwas = buffer.region(Loc(0, 4), Loc(0, 8))
    val rwassize = buffer.lines.size
    buffer.insert(Loc(0, 4), rwas)
    assertEquals(rwassize, buffer.lines.size)
    assertEquals("Who was was that man?", buffer.line(0).asString)

    // insert a ragged region and make sure the first and last lines are properly merged (the code
    // path executed is the same as i0 above, but hey, writing test code is fun!)
    val tbuffer = testBuffer(testText)
    val i3size = tbuffer.lines.size + r3.size - 1
    tbuffer.insert(Loc(0, 16), r3)
    assertEquals(i3size, tbuffer.lines.size)
    assertEquals(Seq(WHO, NOW, "Every?", NOW), tbuffer.lines.slice(0, 4).map(_.asString))

    // finally make sure the originally extracted regions didn't change out from under us
    checkregions()
  }

  @Test def testSingleLineSearch () {
    val buffer = testBuffer(testText)
    val the = Seq(Line("the"))
    // search for all the thes in the whole buffer
    val allThes = buffer.search(the, buffer.start, buffer.end)
    assertEquals(5, allThes.size)
    // search for thes on the first line (should be none)
    val l2 = Loc(1, 0)
    val firstThes = buffer.search(the, buffer.start, l2)
    assertEquals(0, firstThes.size)
    // search for thes in the first half of the second line (should be one)
    val l2half = Loc(1, 20)
    val firstHalfThes = buffer.search(the, l2, l2half)
    assertEquals(1, firstHalfThes.size)
    // search for thes in the second line just before the second 'the' (should be one)
    val l2most = Loc(1, 51)
    // println(buffer.region(l2, l2most).map(_.asString).mkString)
    val firstMostThes = buffer.search(the, l2, l2most)
    assertEquals(1, firstMostThes.size)
    // search for thes in the second line just including the second 'the' (should be two)
    val l2mostp = Loc(1, 52)
    // println(buffer.region(l2, l2mostp).map(_.asString).mkString)
    val firstMostPThes = buffer.search(the, l2, l2mostp)
    assertEquals(2, firstMostPThes.size)
    // search for thes on the whole second line (should be three)
    val l3 = Loc(2, 0)
    val secondThes = buffer.search(the, l2, l3)
    assertEquals(3, secondThes.size)
    // search for at most two thes on the whole second line (should be two)
    val secondLimitThes = buffer.search(the, l2, l3, 2)
    assertEquals(2, secondLimitThes.size)
  }

  // TODO: @Test def testMultiLineSearch () {}
}
