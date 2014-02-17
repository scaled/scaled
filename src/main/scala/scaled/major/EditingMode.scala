//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A base class for all modes that support interactive text editing. This mode defines all of the
  * basic cursor movement and text editing commands. Most major modes will inherit from this mode.
  */
abstract class EditingMode extends MajorMode {

  override def keymap = Seq(
    "C-f" -> "forward-char",
    "C-b" -> "backward-char"
  )

  @Fn("Moves the point forward one character.")
  def forwardChar (view :BufferView) {
    // TODO: more efficient version that makes use of line offset
    view.point = view.buffer.loc(view.point.offset+1)
  }

  @Fn("Moves the point backward one character.")
  def backwardChar (view :BufferView) {
    // TODO: more efficient version that makes use of line offset
    view.point = view.buffer.loc(view.point.offset-1)
  }
}
