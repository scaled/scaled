//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import org.junit._
import org.junit.Assert._

class LocTest {

  def testInsert (descrip :String, expect :Loc, loc :Loc, start :Loc, end :Loc) {
    assertEquals(s"$descrip $loc +~ [$start, $end)", expect, Loc.adjustForInsert(loc, start, end))
  }

  def testDelete (descrip :String, expect :Loc, loc :Loc, start :Loc, end :Loc) {
    assertEquals(s"$descrip $loc -~ [$start, $end)", expect, Loc.adjustForDelete(loc, start, end))
  }

  @Test def testAdjustForInsert () {
    testInsert("Insert after loc should not affect it", Loc(5, 5),
               Loc(5, 5), Loc(5, 6), Loc(5, 9))
    testInsert("Insert before loc row should only affect row", Loc(7, 5),
               Loc(5, 5), Loc(3, 0), Loc(5, 4))
    testInsert("Single line insert on loc row should slide loc over", Loc(5, 9),
               Loc(5, 5), Loc(5, 0), Loc(5, 4))
    testInsert("Multi line insert on loc row should move loc", Loc(6, 8),
               Loc(5, 5), Loc(5, 1), Loc(6, 4))
    testInsert("Insert exactly at loc should move it", Loc(5, 7),
               Loc(5, 5), Loc(5, 5), Loc(5, 7))
  }

  @Test def testAdjustForDelete () {
    testDelete("Delete after loc should not affect it", Loc(5, 5),
               Loc(5, 5), Loc(5, 6), Loc(5, 9))
    testDelete("Delete before loc row should only affect row", Loc(3, 5),
               Loc(5, 5), Loc(2, 0), Loc(4, 4))
    testDelete("Single line delete on loc row should slide loc over", Loc(5, 1),
               Loc(5, 5), Loc(5, 0), Loc(5, 4))
    testDelete("Multi line delete ending on loc row should move loc", Loc(4, 2),
               Loc(5, 5), Loc(4, 1), Loc(5, 4))
    testDelete("Delete exactly at loc should not move it", Loc(5, 5),
               Loc(5, 5), Loc(5, 5), Loc(5, 7))
    testDelete("Single line delete surrounding loc should move it to start", Loc(5, 2),
               Loc(5, 5), Loc(5, 2), Loc(5, 7))
    testDelete("Multi line delete surrounding loc should move it to start", Loc(4, 5),
               Loc(5, 5), Loc(4, 5), Loc(6, 9))
  }
}
