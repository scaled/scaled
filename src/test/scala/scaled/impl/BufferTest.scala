//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.{File, StringReader}

import org.junit._
import org.junit.Assert._

import scaled._

class BufferTest {

  val text = """Who was that man?
    |Now is the time for all good citizens to come to the aid of their country.
    |Every good boy deserves fudge.
    |The quick brown fox jumped over the lazy dog.
    |""".stripMargin

  def testBuffer (text :String) = BufferImpl("test", new File(""), new StringReader(text))
  @Test def testBasics () {
    val buffer = testBuffer(text)
    assertEquals(5, buffer.lines.size)
  }

  @Test def testLoc () {
    val buffer = testBuffer(text)
    assertEquals(Loc(0, 5), buffer.loc(5))
    assertEquals(5, buffer.offset(Loc(0, 5)))
    assertEquals(Loc(1, 0), buffer.loc(18))
    assertEquals(18, buffer.offset(Loc(1, 0)))
    assertEquals(Loc(1, 4), buffer.loc(22))
    assertEquals(22, buffer.offset(Loc(1, 4)))
    // any offset greater than or equal to the buffer length should
    // resolve to a new blank link at the end of the buffer
    assertEquals(Loc(4, 0), buffer.loc(text.length))
    assertEquals(Loc(4, 0), buffer.loc(text.length+20))
  }

  @Test def testMutate () {
    val buffer = testBuffer(text)
    buffer.delete(1, 1)
    assertTrue(buffer.line(1).asString.startsWith("Every good"))
    buffer.split(1, "Every good".length)
    // TODO: ensure that the proper events are emitted?
    assertEquals("Every good", buffer.line(1).asString)
    assertEquals(" boy deserves fudge.", buffer.line(2).asString)
    buffer.insert(2, " smelling".toCharArray)
    buffer.join(1)
    buffer.join(1)
    assertEquals("Every good smelling boy deserves fudge.", buffer.line(1).asString)
  }

  @Test def testMotion () {
    val buffer = testBuffer(text)
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
}
