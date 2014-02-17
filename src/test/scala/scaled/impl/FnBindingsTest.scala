//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit._
import org.junit.Assert._

import scaled.major.TextMode

class FnBindingsTest {

  @Test def testCollectBindings () {
    val mode = new TextMode(null)
    val binds = new FnBindings(mode)
    assertTrue(binds.binding("forward-char").isDefined)
    assertTrue(binds.binding("backward-char").isDefined)
    assertFalse(binds.binding("peanut").isDefined)
    binds.bindings foreach println
  }
}
