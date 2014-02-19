//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A base class for all modes that support interactive text editing. This mode defines all of the
  * basic cursor movement and text editing commands. Most major modes will inherit from this mode.
  */
abstract class EditingMode (view :RBufferView) extends MajorMode {

  override def keymap = Seq(
    "C-f"    -> "forward-char",
    "C-b"    -> "backward-char",
    "UP"     -> "scroll-up",
    "DOWN"   -> "scroll-down",
    "S-UP"   -> "scroll-up-page",
    "S-DOWN" -> "scroll-down-page"
  )

  @Fn("Moves the point forward one character.")
  def forwardChar () {
    // TODO: more efficient version that makes use of line offset
    view.point = view.buffer.loc(view.point.offset+1)
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    // TODO: more efficient version that makes use of line offset
    view.point = view.buffer.loc(view.point.offset-1)
  }

  @Fn("Scrolls the buffer up one line.")
  def scrollUp () {
    view.scrollVert(-1)
  }

  @Fn("Scrolls the buffer down one line.")
  def scrollDown () {
    view.scrollVert(1)
  }

  @Fn("Scrolls the buffer up one page.")
  def scrollUpPage () {
    view.scrollVert(-(view.height-1))
  }

  @Fn("Scrolls the buffer down one page.")
  def scrollDownPage () {
    view.scrollVert(view.height-1)
  }
}
