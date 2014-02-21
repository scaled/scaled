//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A base class for all modes that support interactive text editing. This mode defines all of the
  * basic cursor movement and text editing commands. Most major modes will inherit from this mode.
  */
abstract class EditingMode (view :RBufferView) extends MajorMode {

  private[this] val buffer = view.buffer

  override def keymap = Seq(
    "C-b"    -> "backward-char",
    "C-f"    -> "forward-char",
    "LEFT"   -> "backward-char",
    "RIGHT"  -> "forward-char",

    "C-a"    -> "move-beginning-of-line",
    "C-e"    -> "move-end-of-line",
    "HOME"   -> "move-beginning-of-line",
    "END"    -> "move-end-of-line",

    "C-p"    -> "previous-line",
    "C-n"    -> "next-line",
    "UP"     -> "previous-line",
    "DOWN"   -> "next-line",

    "S-UP"   -> "scroll-up", // TODO: extend-mark-backward-line
    "S-DOWN" -> "scroll-down", // TODO: extend-mark-forward-line
    "M-UP"   -> "scroll-up-page",
    "M-DOWN" -> "scroll-down-page"
  )

  @Fn("Moves the point forward one character.")
  def forwardChar () {
    val old = view.point
    // if we're at the end of the current line, move to the next line
    view.point = if (old.col == buffer.lineLength(old)) Loc(old.row+1, 0)
                 else old + (0, 1)
    // if the point didn't change, that means we tried to move past the end of the buffer
    if (old == view.point) view.emitStatus("End of buffer.")
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val old = view.point
    view.point = if (old.col == 0) Loc(old.row-1, buffer.lineLength(old.row-1))
                 else old + (0, -1)
    if (old == view.point) view.emitStatus("Beginning of buffer.")
  }

  @Fn("Moves the point down one line.")
  def nextLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old + (1, 0)
    if (old == view.point) view.emitStatus("End of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old + (-1, 0)
    if (old == view.point) view.emitStatus("Beginning of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point to the beginning of the line.")
  def moveBeginningOfLine () = view.point = view.point.atCol(0)

  @Fn("Moves the point to the end of the line.")
  def moveEndOfLine () = view.point = view.point.atCol(buffer.line(view.point).length)

  @Fn("Scrolls the view up one line.")
  def scrollUp () = view.scrollVert(-1)

  @Fn("Scrolls the view down one line.")
  def scrollDown () = view.scrollVert(1)

  @Fn("Scrolls the view up one page.")
  def scrollUpPage () = view.scrollVert(-(view.height-1))

  @Fn("Scrolls the view down one page.")
  def scrollDownPage () = view.scrollVert(view.height-1)
}
