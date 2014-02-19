//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.input.KeyCode

import org.junit._
import org.junit.Assert._

class KeyPressTest {

  val T = true
  val F = false

  @Test def testToKeyPress () {
    assertEquals(Some(KeyPress(KeyCode.A, T, F, F, F)), KeyPress.toKeyPress("S-a"))
    assertEquals(Some(KeyPress(KeyCode.A, F, T, F, F)), KeyPress.toKeyPress("C-a"))
    assertEquals(Some(KeyPress(KeyCode.A, F, F, T, F)), KeyPress.toKeyPress("A-a"))
    assertEquals(Some(KeyPress(KeyCode.A, F, F, F, T)), KeyPress.toKeyPress("M-a"))
    assertEquals(Some(KeyPress(KeyCode.A, T, T, F, F)), KeyPress.toKeyPress("S-C-a"))
    assertEquals(Some(KeyPress(KeyCode.A, T, T, F, F)), KeyPress.toKeyPress("C-S-a"))
    assertEquals(Some(KeyPress(KeyCode.A, T, T, F, F)), KeyPress.toKeyPress("C-S-a"))
    assertEquals(Some(KeyPress(KeyCode.A, T, T, T, T)), KeyPress.toKeyPress("S-C-A-M-a"))
    assertEquals(Some(KeyPress(KeyCode.Z, F, T, F, F)), KeyPress.toKeyPress("C-z"))
    assertEquals(Some(KeyPress(KeyCode.TAB, F, F, F, F)), KeyPress.toKeyPress("TAB"))
    assertEquals(None, KeyPress.toKeyPress("BOB"))
    assertEquals(None, KeyPress.toKeyPress("G-z"))
  }
}
