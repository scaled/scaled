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
    val cp = view.point
    view.point = if (cp.col < buffer.line(cp).length) cp + (0, 1)
                 else Loc(cp.row+1, 0) // move down a line
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val cp = view.point
    view.point = if (cp.col > 0) cp + (0, -1)
                 else Loc(cp.row-1, buffer.lines(cp.row-1).length) // move up a line
  }

  @Fn("Moves the point down one line.")
  def nextLine () = view.point.row match {
    case row if (row < buffer.lines.length) =>
      // if we're on the last line of the buffer, move to start of blank line following the buffer
      if (row == buffer.lines.length-1) view.point = Loc(0, row+1)
      // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
      // means not bounding the column, but instead coping with a current column that is beyond
      // the end of the current line (will that be a pandora's box?)
      else view.point = Loc(row+1, math.min(row, buffer.lines(row+1).length))
    case _ => view.emitStatus("End of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () = view.point.row match {
    case row if (row > 0) =>
      // TODO: see nextLine about preserving "intended" column
      view.point = Loc(row-1, math.min(row, buffer.lines(row-1).length))
    case _ => view.emitStatus("Beginning of buffer.") // TODO: with beep?
  }

  @Fn("Scrolls the view up one line.")
  def scrollUp () = view.scrollVert(-1)

  @Fn("Scrolls the view down one line.")
  def scrollDown () = view.scrollVert(1)

  @Fn("Scrolls the view up one page.")
  def scrollUpPage () = view.scrollVert(-(view.height-1))

  @Fn("Scrolls the view down one page.")
  def scrollDownPage () = view.scrollVert(view.height-1)
}
