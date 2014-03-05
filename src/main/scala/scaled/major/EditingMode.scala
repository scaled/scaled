//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec

import scaled._

/** A base class for all modes that support interactive text editing. This mode defines all of the
  * basic cursor movement and text editing commands. Most major modes will inherit from this mode.
  */
abstract class EditingMode (view :RBufferView) extends MajorMode {

  private[this] val buffer = view.buffer

  /** The syntax table in use for this mode. */
  val syntax :SyntaxTable = createSyntaxTable()

  /** Creates the syntax table used for this mode. Defaults to the default syntax table. A major mode
    * with special syntax should override this method and return a customized syntax table. */
  protected def createSyntaxTable () = new SyntaxTable()

  override def keymap = Seq(
    "BS"    -> "delete-backward-char", // TODO: make this delete back to mark (if set)
    "DEL"   -> "delete-forward-char", // ...forward to mark (if set)
    "C-d"   -> "delete-forward-char", // this should be delete-char and ignore mark

    "ENTER" -> "newline",
    // TODO: open-line, split-line, ...

    "C-t"    -> "transpose-chars",

    // mark manipulation commands
    "C-SPACE" -> "set-mark-command", // TODO: make this push-mark instead?
    "C-@"     -> "set-mark-command",
    "C-x C-x" -> "exchange-point-and-mark",

    // killing and yanking commands
    // "C-k"     -> "kill-line",
    // "C-S-BS"  -> "kill-whole-line",
    // "C-w"     -> "kill-region",
    // "M-w"     -> "kill-ring-save", // do we care about copy-region-as-kill? make it an alias?
    // "M-d"     -> "kill-word",
    // "M-DEL"   -> "backward-kill-word", // also "C-BS"
    // "M-z"     -> "zap-to-char",
    // "M-k"     -> "kill-sentence", // do we want?
    // "C-x DEL" -> "backward-kill-sentence", // do we want?
    // "C-M-k"   -> "kill-balanced-sexp", // do we want?

    // "C-y" -> "yank"
    // "M-y" -> "yank-pop"

    // undo commands
    "C-/"   -> "undo",
    "C-\\"  -> "redo",
    "C-x r" -> "redo",
    "C-x u" -> "undo",
    "C-S--" -> "undo", // TODO: make C-_ work

    // motion commands
    "C-b"   -> "backward-char",
    "C-f"   -> "forward-char",
    "LEFT"  -> "backward-char",
    "RIGHT" -> "forward-char",

    "M-b"     -> "backward-word",
    "M-f"     -> "forward-word",
    "C-LEFT"  -> "backward-word",
    "C-RIGHT" -> "forward-word",

    "C-a"  -> "move-beginning-of-line",
    "C-e"  -> "move-end-of-line",
    "HOME" -> "move-beginning-of-line",
    "END"  -> "move-end-of-line",

    "C-p"  -> "previous-line",
    "C-n"  -> "next-line",
    "UP"   -> "previous-line",
    "DOWN" -> "next-line",

    "C-UP"   -> "previous-paragraph",
    "C-DOWN" -> "next-paragraph",

    "S-UP"   -> "scroll-up", // TODO: extend-mark-backward-line
    "S-DOWN" -> "scroll-down", // TODO: extend-mark-forward-line
    "M-v"    -> "scroll-up-page",
    "C-v"    -> "scroll-down-page"
  )

  @Fn("Moves the point forward one character.")
  def forwardChar () {
    val old = view.point
    // if we're at the end of the current line, move to the next line
    view.point = buffer.forward(old, 1)
    // if the point didn't change, that means we tried to move past the end of the buffer
    if (old == view.point) view.emitStatus("End of buffer.")
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val old = view.point
    view.point = buffer.backward(old, 1)
    if (old == view.point) view.emitStatus("Beginning of buffer.")
  }

  @Fn("Deletes the character immediately previous to the point.")
  def deleteBackwardChar () {
    val vp = view.point
    val prev = buffer.backward(vp, 1)
    if (prev == vp) view.emitStatus("Beginning of buffer.")
    else {
      view.point = prev // move the point back one space
      if (vp.col == 0) buffer.join(prev.row) // join the previous line to this one
      else buffer.delete(prev, 1) // delete the previous character
    }
  }

  @Fn("Deletes the character at the point.")
  def deleteForwardChar () {
    val del = view.point
    if (del.row >= buffer.lines.size) view.emitStatus("End of buffer.")
    else if (buffer.lineLength(del) == del.col) buffer.join(del.row)
    else buffer.delete(del, 1)
  }

  @Fn("""Swaps the character at the point with the character preceding it, and moves the point
         forward one character. If the point is at the start of a line, the character will be
         'transposed' with the newline preceding it, effectively moving the character to the
         previous line. If the point is past the end of a line, the character at the end of the
         line will be transposed with the preceding character.""")
  def transposeChars () {
    // the passages below are a bit twisty, but it (mostly) mimics what emacs does
    val p = view.point
    val lineLen = buffer.lineLength(p)
    // if the point is past the last char, act as if it's on the last char
    val tp = if (lineLen > 0 && p.col >= lineLen) p.atCol(lineLen-1) else p
    // if we're at the start of the buffer, this command is meaningless
    if (tp == Loc.Zero) view.emitStatus("Beginning of buffer.")
    // if the point is in column zero...
    else if (tp.col == 0) {
      val prev = tp.row - 1
      // transpose the first character of this line with the preceding line's separator (push our
      // first character onto the end of the previous line)
      if (lineLen > 0) {
        val p0 = buffer.start(p)
        buffer.insert(buffer.end(prev), buffer.charAt(p0))
        buffer.delete(p0, 1)
        // in this case we don't bump the point fwd because it's already "after" the moved char
      }
      // unless the current line has no characters...
      else buffer.lineLength(prev) match {
        // if the previous line is also an empty line, we got nothing
        case 0 =>  view.emitStatus("Nothing to transpose.")
        // otherwise pull the last character of the previous line into this one
        case len =>
          val last = Loc(prev, len-1)
          buffer.insert(buffer.start(p), buffer.charAt(last))
          buffer.delete(last, 1)
          view.point = tp.nextC
      }
    }
    // otherwise we have a normal transpose: swap the char under the point with the prev char
    else {
      val swap = tp.prevC
      buffer.replace(swap, 2, new Line(Array(buffer.charAt(tp), buffer.charAt(swap))))
      view.point = tp.nextC
    }
  }

  @Fn("Moves the point forward one word.")
  def forwardWord () {
    // move forward until we see a word char, then keep going until we see a non-word char and stop
    val end = buffer.end
    @tailrec def seek (pos :Loc, seenWord :Boolean) :Loc = syntax(buffer.charAt(pos)) match {
      case Syntax.Word => seek(buffer.forward(pos, 1), true)
      case _           => if (seenWord || pos == end) pos else seek(buffer.forward(pos, 1), false)
    }
    view.point = seek(view.point, false)
  }

  @Fn("Moves the point backward one word.")
  def backwardWord () {
    // move backward until we see a word char, and then keep going until we see a non-word char and
    // stop on the word char we saw just before
    val start = buffer.start
    @tailrec def seek (pos :Loc, seenWord :Boolean) :Loc = if (pos == start) start else {
      val ppos = buffer.backward(pos, 1)
      syntax(buffer.charAt(ppos)) match {
        case Syntax.Word => seek(ppos, true)
        case _           => if (seenWord) pos else seek(ppos, false)
      }
    }
    view.point = seek(view.point, false)
  }

  @Fn("""Inserts a newline at the point.
         Characters after the point on the current line wil be moved to a new line.""")
  def newline () {
    buffer.split(view.point)
    view.point = Loc(view.point.row+1, 0)
  }

  @Fn("Sets the mark to the current point.")
  def setMarkCommand () {
    // TODO: push old mark onto local (buffer?) and global mark ring?
    buffer.mark = view.point
    view.emitStatus("Mark set.")
  }

  @Fn("Sets the mark to the current point and moves the point to the previous mark.")
  def exchangePointAndMark () {
    buffer.mark match {
      case Some(m) =>
        buffer.mark = view.point
        view.point = m
      case None =>
        view.emitStatus("No mark set in this buffer.")
    }
  }

  @Fn("Undoes the last change to the buffer.")
  def undo () {
    if (!view.undoer.undo()) view.emitStatus("Nothing to undo.")
  }

  @Fn("Redoes the last undone to the buffer.")
  def redo () {
    if (!view.undoer.redo()) view.emitStatus("Nothing to redo.")
  }

  @Fn("Moves the point down one line.")
  def nextLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old.nextL
    if (old == view.point) view.emitStatus("End of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old.prevL
    if (old == view.point) view.emitStatus("Beginning of buffer.") // TODO: with beep?
  }

  // TODO: add config: paragraph-ignore-whitespace and treat non-empty lines which contain only
  // whitespace as paragraph delimiters
  @Fn("""Moves to the next paragraph. Paragraphs are currently delimited by blank lines.
         TODO: make this more emacs-like?""")
  def nextParagraph () {
    @tailrec def seek (row :Int, seenNonBlank :Boolean) :Loc = {
      if (row >= buffer.lines.size) Loc(row, 0)
      else {
        val len = buffer.lineLength(row)
        if (len == 0 && seenNonBlank) Loc(row, 0)
        else seek(row+1, seenNonBlank || len > 0)
      }
    }
    view.point = seek(view.point.row, false)
  }

  @Fn("""Moves to the previous paragraph. Paragraphs are currently delimited by blank lines.
         TODO: make this more emacs-like?""")
  def previousParagraph () {
    @tailrec def seek (row :Int, seenNonBlank :Boolean) :Loc = {
      if (row <= 0) Loc(0, 0)
      else {
        val len = buffer.lineLength(row)
        if (len == 0 && seenNonBlank) Loc(row, 0)
        else seek(row-1, seenNonBlank || len > 0)
      }
    }
    view.point = seek(view.point.row, false)
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
