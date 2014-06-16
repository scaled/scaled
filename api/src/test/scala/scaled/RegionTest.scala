//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class RegionTest {

  @Test def testFind () {
    val r1 = Region(Loc(0, 0), Loc(0, 5))
    val r2 = Region(Loc(0, 8), Loc(0, 13))
    val r3 = Region(Loc(1, 0), Loc(1, 7))
    val r4 = Region(Loc(2, 1), Loc(3, 15))
    val r5 = Region(Loc(3, 15), Loc(4, 1))
    val rs = Array(r1, r2, r3, r4, r5)

    assertEquals(Some(r1), Region.find(rs, Loc(0, 0)))
    assertEquals(Some(r1), Region.find(rs, Loc(0, 3)))
    assertEquals(None, Region.find(rs, Loc(0, 5)))
    assertEquals(Some(r2), Region.find(rs, Loc(0, 8)))
    assertEquals(Some(r2), Region.find(rs, Loc(0, 10)))
    assertEquals(None, Region.find(rs, Loc(0, 13)))
    assertEquals(Some(r3), Region.find(rs, Loc(1, 0)))
    assertEquals(Some(r3), Region.find(rs, Loc(1, 5)))
    assertEquals(None, Region.find(rs, Loc(1, 9)))
    assertEquals(None, Region.find(rs, Loc(2, 0)))
    assertEquals(Some(r4), Region.find(rs, Loc(2, 6)))
    assertEquals(Some(r4), Region.find(rs, Loc(3, 14)))
    assertEquals(Some(r5), Region.find(rs, Loc(3, 15)))
    assertEquals(Some(r5), Region.find(rs, Loc(4, 0)))
    assertEquals(None, Region.find(rs, Loc(4, 1)))
    assertEquals(None, Region.find(rs, Loc(4, 15)))
    assertEquals(None, Region.find(rs, Loc(5, 1)))

    assertEquals(None, Region.find(Array[Region](), Loc(3, 3)))
  }
}
