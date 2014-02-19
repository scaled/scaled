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
    // TODO: more efficient version that makes use of line offset
    view.point = buffer.loc(view.point.offset+1)
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    // TODO: more efficient version that makes use of line offset
    view.point = buffer.loc(view.point.offset-1)
  }

  @Fn("Moves the point down one line.")
  def nextLine () = view.point.lineIdx match {
    case lidx if (lidx < buffer.lines.length) =>
      if (lidx == buffer.lines.length-1) view.point = Loc(buffer.length
      // TODO: see previousLine about preserving "intended" column
      val next = buffer.lines(lidx+1)
      val nlidx = math.min(lidx, next.length) // bound the point in the next line
      val delta = nlidx + (buffer.lines(lidx).length - lidx)
      view.point = Loc(view.point.offset+delta, lidx+1, nlidx)
    case _ => view.emitStatus("Beginning of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () = view.point.lineIdx match {
    case lidx if (lidx > 0) =>
      // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
      // means not bounding the column, but instead coping with a current column that is beyond the
      // end of the current line (will that be a pandora's box?)
      val prev = buffer.lines(lidx-1)
      val plidx = math.min(lidx, prev.length) // bound the point in the previous line
      val delta = (prev.length - plidx) + lidx
      view.point = Loc(view.point.offset-delta, lidx-1, plidx)
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
