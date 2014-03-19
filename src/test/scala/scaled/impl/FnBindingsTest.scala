//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import org.junit._
import org.junit.Assert._

import scaled.major.TextMode

class FnBindingsTest {

  @Test def testCollectBindings () {
    val view = new BufferViewImpl(TestData.editor, TestData.buffer("test", ""), 80, 24)
    val disp = new DispatcherImpl(TestData.editor, view) {
      override def createMode () = new TextMode(TestData.editor, TestData.config, view, this)
    }
    val binds = new FnBindings(disp.major, System.err.println)
    // binds.bindings foreach println
    assertTrue(binds.binding("forward-char").isDefined)
    assertTrue(binds.binding("backward-char").isDefined)
    assertFalse(binds.binding("peanut").isDefined)
  }
}
