//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit._
import org.junit.Assert._

import scaled.Editor
import scaled.major.TextMode

class FnBindingsTest {

  // TODO: move to test utils object
  val editor = new Editor {
    val killRing = new KillRingImpl(10)
    def showURL (url :String) {}
    def exit (code :Int) {}
  }

  @Test def testCollectBindings () {
    val mode = new TextMode(editor, new BufferViewImpl(TestData.buffer("test", "")))
    val binds = new FnBindings(mode)
    // binds.bindings foreach println
    assertTrue(binds.binding("forward-char").isDefined)
    assertTrue(binds.binding("backward-char").isDefined)
    assertFalse(binds.binding("peanut").isDefined)
  }
}
