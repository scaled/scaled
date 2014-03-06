//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit._
import org.junit.Assert._

import scaled._

class KillRingTest {

  @Test def testRingness () {
    val buf = BufferTest.testBuffer()
    val ring = new KillRingImpl(8)
    val rega = buf.region(Loc(0, 0), Loc(1, 0))
    val regb = buf.region(Loc(1, 0), Loc(2, 0))
    val regc = buf.region(Loc(2, 0), Loc(3, 0))

    // make sure the region starts out empty
    0 until 16 foreach { ii => assertEquals(None, ring.entry(ii)) }

    // add a, b and c; ensure that we get them back in reverse order
    Seq(rega, regb, regc) foreach ring.add
    Seq(regc, regb, rega).zipWithIndex foreach {
      case (r, idx) => assertEquals(Some(r), ring.entry(idx)) }

    // make sure we wrap around the ring properly
    Seq(regc, regb, rega, regc, regb, rega, regc, regb, rega).zipWithIndex foreach {
      case (r, idx) => assertEquals(Some(r), ring.entry(idx)) }

    // fill the ring with as and make sure they overwrite everything else
    0 until 8 foreach { _ => ring.add(rega) }
    0 until 8 foreach { ii => assertEquals(Some(rega), ring.entry(ii)) }
  }

  @Test def testAppend () {
    val buf = BufferTest.testBuffer()
    val ring = new KillRingImpl(8)
    val (l1, l2, l3) = (Loc(0, 0), Loc(1, 0), Loc(2, 0))
    ring.add(buf.region(l1, l2))
    ring.append(buf.region(l2, l3))
    assertEquals(Some(buf.region(l1, l3)), ring.entry(0))
  }
}
