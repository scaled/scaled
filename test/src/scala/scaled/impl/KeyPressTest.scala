//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.input.KeyCode
import org.junit.Assert._
import org.junit._
import scaled._

class KeyPressTest {
  import KeyPress._

  val T = true
  val F = false

  @Test def testToKeyPress () {
    assertEquals(Some(fromCode(KeyCode.A, T, F, F, F)), toKeyPress("S-a"))
    assertEquals(Some(fromCode(KeyCode.A, F, T, F, F)), toKeyPress("C-a"))
    assertEquals(Some(fromCode(KeyCode.A, F, F, T, F)), toKeyPress("A-a"))
    assertEquals(Some(fromCode(KeyCode.A, F, F, F, T)), toKeyPress("M-a"))
    assertEquals(Some(fromCode(KeyCode.A, T, T, F, F)), toKeyPress("S-C-a"))
    assertEquals(Some(fromCode(KeyCode.A, T, T, F, F)), toKeyPress("C-S-a"))
    assertEquals(Some(fromCode(KeyCode.A, T, T, F, F)), toKeyPress("C-S-a"))
    assertEquals(Some(fromCode(KeyCode.A, T, T, T, T)), toKeyPress("S-C-A-M-a"))
    assertEquals(Some(fromCode(KeyCode.Z, F, T, F, F)), toKeyPress("C-z"))
    assertEquals(Some(fromCode(KeyCode.X, T, T, F, F)), toKeyPress("S-C-x"))
    assertEquals(Some(fromCode(KeyCode.TAB, F, F, F, F)), toKeyPress("TAB"))
    assertEquals(None, KeyPress.toKeyPress("BOB"))
    assertEquals(None, KeyPress.toKeyPress("G-z"))
  }
}
