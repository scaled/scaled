//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import java.io.File

import reactual.Future

import scala.annotation.tailrec

import scaled._

/** A base class for all modes that support interactive text editing. This mode defines all of the
  * basic cursor movement and text editing commands. Most major modes will inherit from this mode.
  */
abstract class EditingMode (editor :Editor, view :RBufferView, disp :Dispatcher) extends MajorMode {

  @inline protected def buffer = view.buffer // for great brevity

  /** The syntax table in use for this mode. */
  val syntax :SyntaxTable = createSyntaxTable()

  /** Creates the syntax table used for this mode. Defaults to the default syntax table. A major mode
    * with special syntax should override this method and return a customized syntax table. */
  protected def createSyntaxTable () = new SyntaxTable()

  override def defaultFn = Some("self-insert-command")

  override def keymap = Seq(
    // character editing commands
    "BS"    -> "delete-backward-char", // TODO: make this delete back to mark (if set)
    "DEL"   -> "delete-forward-char", // ...forward to mark (if set)
    "C-d"   -> "delete-forward-char", // this should be delete-char and ignore mark

    "ENTER" -> "newline",
    "TAB"   -> "indent-for-tab-command",
    // TODO: open-line, split-line, ...

    "C-t"    -> "transpose-chars",

    // mark manipulation commands
    "C-SPACE" -> "set-mark-command", // TODO: make this push-mark instead?
    "C-@"     -> "set-mark-command", // needs to be C-S-2? meh.
    "C-x C-x" -> "exchange-point-and-mark",

    // killing and yanking commands
    "C-w"     -> "kill-region",
    "C-M-w"   -> "append-next-kill",
    "M-w"     -> "kill-ring-save", // do we care about copy-region-as-kill? make it an alias?
    "C-k"     -> "kill-line",
    "C-S-BS"  -> "kill-whole-line",
    "M-d"     -> "kill-word",
    "M-DEL"   -> "backward-kill-word",
    "C-BS"    -> "backward-kill-word",
    // "M-z"     -> "zap-to-char",
    // "M-k"     -> "kill-sentence", // do we want?
    // "C-x DEL" -> "backward-kill-sentence", // do we want?
    // "C-M-k"   -> "kill-balanced-sexp", // do we want?

    "C-y" -> "yank",
    "M-y" -> "yank-pop",

    // undo commands
    "C-/"   -> "undo",
    "C-\\"  -> "redo",
    "C-x r" -> "redo",
    "C-x u" -> "undo",
    "C-_"   -> "undo",
    // TEMP: until we sort out ctrl'd shifted keys
    "C-S--" -> "undo",

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

    "M-<"    -> "beginning-of-buffer",
    "M->"    -> "end-of-buffer",
    "C-HOME" -> "beginning-of-buffer",
    "C-END"  -> "end-of-buffer",
    "BEGIN"  -> "beginning-of-buffer",
    // TEMP: until we sort out meta'd shifted keys
    "M-S-,"  -> "beginning-of-buffer",
    "M-S-."  -> "end-of-buffer",

    "M-g"    -> "goto-line",

    // view commands (scrolling, etc.)
    "S-UP"   -> "scroll-up", // TODO: extend-mark-backward-line
    "S-DOWN" -> "scroll-down", // TODO: extend-mark-forward-line
    "M-v"    -> "scroll-up-page",
    "C-v"    -> "scroll-down-page",
    "PGUP"   -> "scroll-up-page",
    "PGDN"   -> "scroll-down-page",

    "C-l"    -> "recenter",

    // buffer commands
    "C-x b"   -> "switch-to-buffer",
    "C-x k"   -> "kill-buffer",
    "C-x C-s" -> "save-buffer",
    "C-x C-f" -> "find-file",
    "C-x C-w" -> "write-file",

    // editor commands
    "C-x C-c" -> "save-buffers-kill-editor",

    // meta commands
    "M-x" -> "execute-extended-command"
  )

  /** Seeks forward to the end a word. Moves forward from `p` until at least one word char is seen,
    * and then keeps going until a non-word char is seen (or the end of the buffer is reached), and
    * that point is returned.
    */
  def forwardWord (p :Loc) :Loc = {
    val end = buffer.end
    @tailrec def seek (pos :Loc, seenWord :Boolean) :Loc = syntax(buffer.charAt(pos)) match {
      case Syntax.Word => seek(buffer.forward(pos, 1), true)
      case _           => if (seenWord || pos == end) pos else seek(buffer.forward(pos, 1), false)
    }
    seek(p, false)
  }

  /** Seeks backward to the start of a word. Moves backward from `p` until at least one word char is
    * seen (whether `p` is a word char is not relevant), and then keeps going until a non-word char
    * is seen (or the start of the buffer is reached), and returns the loc of the last seen word
    * char.
    */
  def backwardWord (p :Loc) :Loc = {
    val start = buffer.start
    @tailrec def seek (pos :Loc, seenWord :Boolean) :Loc = if (pos == start) start else {
      val ppos = buffer.backward(pos, 1)
      syntax(buffer.charAt(ppos)) match {
        case Syntax.Word => seek(ppos, true)
        case _           => if (seenWord) pos else seek(ppos, false)
      }
    }
    seek(p, false)
  }

  /** Delets the region from `from` to `to` from the buffer and adds it to the kill-ring. If `to` is
    * earlier in the buffer than `from` the arguments will automatically be swapped.
    *
    * Uses `disp.curFn` and `disp.prevFn` to determine whether this kill is due to a repeated
    * command, in which case the killed region is appended to the most recently killed region
    * instead of added to the kill-ring as a new entry.
    *
    * @return the start of the killed region (the smaller of `from` and `to`).
    */
  def kill (from :Loc, to :Loc) :Loc = {
    if (from != to) {
      // delete handles swapping from/to as needed
      val region = buffer.delete(from, to)
      // if the previous fn was any sort of kill fn, we append instead of add
      if ((disp.prevFn != null) && isKillFn(disp.prevFn)) {
        if (isBackwardKill(disp.curFn)) editor.killRing prepend region
        else                            editor.killRing append  region
      }
      // otherwise create a new kill ring entry
      else editor.killRing add region
    }
    from lesser to
  }

  /** Used by [[kill]] to determine if the previous fn was a kill fn, in which case the kill will be
    * appended to the previous kill rather than used to create a new kill-ring entry.
    *
    * Defaults to fns that start with `kill-`, end with `-kill` or contain `-kill-`. Modes may wish
    * to customize this behavior if they introduce kill commands that do not follow this naming
    * scheme, but wish for them to participate in kill accumulation.
    */
  def isKillFn (name :String) :Boolean =
    (name startsWith "kill-") || (name endsWith "-kill") || (name contains "-kill-")

  /** Used by [[kill]] to determine if a fn kills backwards instead of forwards.
    *
    * Defaults to checking whether `name` starts with `backward-`. Modes may wish to customize this
    * behavior if they introduce backwards kill commands that do not follow this naming scheme.
    */
  def isBackwardKill (name :String) = name startsWith "backward-"

  //
  // CHARACTER EDITING FNS

  @Fn("Inserts the character you typed.")
  def selfInsertCommand (typed :String) {
    // insert the typed character at the point
    val p = view.point
    view.buffer.insert(p, typed, Face.defaultFace)
    // move the point to the right by the appropriate amount
    view.point = p + (0, typed.length)
  }

  @Fn("Deletes the character immediately previous to the point.")
  def deleteBackwardChar () {
    val vp = view.point
    val prev = buffer.backward(vp, 1)
    if (prev == vp) editor.emitStatus("Beginning of buffer.")
    else buffer.delete(prev, vp)
    // move the point back one space (TODO: should this be necessary?; I get the idea that buffer
    // deletions prior to the point in Emacs just cause the point to move)
    view.point = prev
  }

  @Fn("Deletes the character at the point.")
  def deleteForwardChar () {
    val del = view.point
    val next = buffer.forward(del, 1)
    if (del == next) editor.emitStatus("End of buffer.")
    else buffer.delete(del, next)
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
    if (tp == Loc.Zero) editor.emitStatus("Beginning of buffer.")
    // if the point is in column zero...
    else if (tp.col == 0) {
      val prev = tp.row - 1
      // transpose the first character of this line with the preceding line's separator (push our
      // first character onto the end of the previous line)
      if (lineLen > 0) {
        val p0 = buffer.lineStart(p)
        val deleted = buffer.delete(p0, 1)
        buffer.insert(buffer.lineEnd(prev), deleted)
        // in this case we don't bump the point fwd because it's already "after" the moved char
      }
      // unless the current line has no characters...
      else buffer.lineLength(prev) match {
        // if the previous line is also an empty line, we got nothing
        case 0 =>  editor.emitStatus("Nothing to transpose.")
        // otherwise pull the last character of the previous line into this one
        case len =>
          val last = Loc(prev, len-1)
          val deleted = buffer.delete(last, 1)
          buffer.insert(buffer.lineStart(p), deleted)
          view.point = tp.nextC
      }
    }
    // otherwise we have a normal transpose: swap the char under the point with the prev char
    else {
      val swap = tp.prevC
      buffer.replace(swap, 2, new Line(Array(buffer.charAt(tp), buffer.charAt(swap)),
                                       Array(buffer.faceAt(tp), buffer.faceAt(swap))))
      view.point = tp.nextC
    }
  }

  //
  // KILLING AND YANKING FNS

  @Fn("""Kills the text between the point and the mark. This removes the text from the buffer and
         adds it to the kill ring. The point and mark are moved to the start of the killed
         region.""")
  def killRegion () = buffer.mark match {
    case None => editor.emitStatus("The mark is not set now, so there is no region.")
    case Some(mp) =>
      view.point = kill(view.point, mp)
      buffer.mark = view.point
  }

  @Fn("""Causes the following command, if it kills, to append to the previous kill rather than
         creating a new kill-ring entry.""")
  def appendNextKill () {
    editor.emitStatus("If next command is a kill, it will append.")
    // kill() will note that the prevFn is append-next-kill and append appropriately
  }

  @Fn("""Saves the text between the point and the mark as if killed, but doesn't kill it.
         The point and mark remain unchanged.""")
  def killRingSave () = buffer.mark match {
    case None => editor.emitStatus("The mark is not set now, so there is no region.")
    case Some(mp) =>
      editor.killRing add buffer.region(view.point, mp)
      editor.emitStatus("Region added to kill ring.")
  }

  @Fn("""Kills the rest of the current line, adding it to the kill ring. If the point is at the end
         of the line, the newline is killed instead.""")
  def killLine () {
    val p = view.point
    val eol = buffer.lineEnd(p)
    // if we're at the end of the line, kill to the first line of the next char (the newline)
    // otherwise kill to the end of the line
    kill(p, if (p == eol) buffer.forward(p, 1) else eol)
  }

  @Fn("""Kills the entire current line.""")
  def killWholeLine () {
    view.point = kill(buffer.lineStart(view.point), buffer.forward(buffer.lineEnd(view.point), 1))
  }

  @Fn("""Kills characters forward until encountering the end of a word.""")
  def killWord () {
    kill(view.point, forwardWord(view.point))
  }

  @Fn("""Kills characters backward until encountering the beginning of a word.""")
  def backwardKillWord () {
    view.point = kill(backwardWord(view.point), view.point)
  }

  @Fn("""Reinserts the most recently killed text. The mark is set to the point and the point is
         moved to the end if the inserted text.""")
  def yank () {
    editor.killRing.entry(0) match {
      case None => editor.emitStatus("Kill ring is empty.")
      case Some(region) =>
        buffer.mark = view.point
        view.point = buffer.insert(view.point, region)
    }
  }

  @Fn("""Replaces the just-yanked stretch of killed text with a different stretch.""")
  def yankPop () {
    if (!yanks(disp.prevFn)) editor.emitStatus(s"Previous command was not a yank (${disp.prevFn}).")
    else {
      yankCount = if (disp.prevFn == "yank-pop") yankCount + 1 else 1
      editor.killRing.entry(yankCount) match {
        case None => editor.emitStatus("Kill ring is empty.")
        case Some(region) =>
          // since the last command was a yank, the mark must be set
          val mark = buffer.mark.get
          buffer.mark = view.point lesser mark
          view.point = buffer.replace(view.point, mark, region)
      }
    }
  }
  private var yankCount = 0
  private val yanks = Set("yank", "yank-pop")

  //
  // MOTION FNS

  @Fn("Moves the point forward one character.")
  def forwardChar () {
    val old = view.point
    // if we're at the end of the current line, move to the next line
    view.point = buffer.forward(old, 1)
    // if the point didn't change, that means we tried to move past the end of the buffer
    if (old == view.point) editor.emitStatus("End of buffer.")
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val old = view.point
    view.point = buffer.backward(old, 1)
    if (old == view.point) editor.emitStatus("Beginning of buffer.")
  }

  @Fn("Moves the point forward one word.")
  def forwardWord () {
    view.point = forwardWord(view.point)
  }

  @Fn("Moves the point backward one word.")
  def backwardWord () {
    view.point = backwardWord(view.point)
  }

  @Fn("""Inserts a newline at the point.
         Characters after the point on the current line wil be moved to a new line.""")
  def newline () {
    buffer.split(view.point)
    view.point = Loc(view.point.row+1, 0)
  }

  @Fn("Indents the current line or region, or inserts a tab, as appropriate.")
  def indentForTabCommand () {
    editor.emitStatus("TODO: tabs!")
  }

  @Fn("Sets the mark to the current point.")
  def setMarkCommand () {
    // TODO: push old mark onto local (buffer?) and global mark ring?
    buffer.mark = view.point
    editor.emitStatus("Mark set.")
  }

  @Fn("Sets the mark to the current point and moves the point to the previous mark.")
  def exchangePointAndMark () {
    buffer.mark match {
      case Some(m) =>
        buffer.mark = view.point
        view.point = m
      case None =>
        editor.emitStatus("No mark set in this buffer.")
    }
  }

  @Fn("Undoes the last change to the buffer.")
  def undo () = buffer.undoer.undo() match {
    case None    => editor.emitStatus("Nothing to undo.")
    case Some(p) => view.point = p
  }

  @Fn("Redoes the last undone to the buffer.")
  def redo () = buffer.undoer.redo() match {
    case None    => editor.emitStatus("Nothing to redo.")
    case Some(p) => view.point = p
  }

  @Fn("Moves the point down one line.")
  def nextLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old.nextL
    if (old == view.point) editor.emitStatus("End of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () {
    val old = view.point
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point = old.prevL
    if (old == view.point) editor.emitStatus("Beginning of buffer.") // TODO: with beep?
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

  @Fn("Moves the point to the beginning of the buffer.")
  def beginningOfBuffer () = view.point = buffer.start

  @Fn("Moves the point to the end of the buffer.")
  def endOfBuffer () = view.point = buffer.end

  @Fn("""Reads line number from minibuffer and goes to that line, counting from line 1 at
         beginning of buffer. Also centers the view on the requested line. If the mark is inactive,
         it will be set to the point prior to moving to the new line. """")
  def gotoLine () {
    editor.miniRead("Goto line:", "", a => Set(a)) onSuccess { lineStr =>
      val line = try { lineStr.toInt } catch {
        case e :Throwable => 1 // this is what emacs does, seems fine to me
      }
      if (!buffer.mark.isDefined) buffer.mark = view.point
      view.point = Loc(line-1, 0)
      recenter()
    }
  }

  //
  // VIEW/SCROLLING FNS

  @Fn("Scrolls the view up one line.")
  def scrollUp () = view.scrollVert(-1)

  @Fn("Scrolls the view down one line.")
  def scrollDown () = view.scrollVert(1)

  @Fn("Scrolls the view up one page.")
  def scrollUpPage () = view.scrollVert(1-view.height)

  @Fn("Scrolls the view down one page.")
  def scrollDownPage () = view.scrollVert(view.height-1)

  @Fn("""Adjusts the scroll offset of the current window so that the line that contains the point
         is centered therein.""")
  def recenter () = view.scrollTopV() = math.max(view.point.row - view.height/2, 0)

  //
  // BUFFER FNS

  @Fn("""Reads a buffer name from the minibuffer and switches to it.""")
  def switchToBuffer () {
    val fstb = editor.buffers.head.name
    val defb = editor.buffers.drop(1).headOption.map(_.name) getOrElse fstb
    val defp = if (defb == "") "" else s" (default $defb)"
    val comp = Completers.buffer(editor, Set(fstb))
    editor.miniRead(s"Switch to buffer$defp:", "", comp) onSuccess { read =>
      editor.openBuffer(if (read == "") defb else read)
    }
  }

  @Fn("""Reads a buffer name from the minibuffer and kills (closes) it.""")
  def killBuffer () {
    val current = editor.buffers.head.name
    val prompt = s"Kill buffer (default $current):"
    editor.miniRead(prompt, "", Completers.buffer(editor)) onSuccess { read =>
      val buffer = if (read == "") current else read
      if (!editor.killBuffer(buffer)) editor.emitStatus(s"No buffer named: $buffer")
    }

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("""Reads a filename from the minibuffer and switches to a filename visiting it.""")
  def findFile () {
    val bufwd = buffer.dir.getAbsolutePath + File.separator
    editor.miniRead("Find file:", bufwd, Completers.file) onSuccess { path =>
      val file = new File(path)
      if (file.isDirectory) editor.emitStatus(
        "Scaled does not support editing directories. Use Emacs.")
      else editor.newBuffer(file)
    }
  }

  @Fn("""Saves current buffer to the currently visited file, if modified.""")
  def saveBuffer () {
    if (!buffer.dirty) editor.emitStatus("No changes need to be saved.")
    else {
      // TODO: all sorts of checks; has the file changed (out from under us) since we loaded it?
      // what else does emacs do?
      buffer.save()
      editor.emitStatus(s"Wrote: ${buffer.file.getAbsolutePath}")
    }
  }

  @Fn("""Saves the buffer to a filename read from the minibuffer. This makes the buffer visit that
         file. If you specify just a directory, the buffer will be saved to its current filename
         in the specified directory.""")
  def writeFile () {
    val bufwd = buffer.dir.getAbsolutePath + File.separator
    editor.miniRead("Write file:", bufwd, Completers.file) onSuccess { path =>
      val file = new File(path).getCanonicalFile
      // require confirmation if another buffer is visiting the specified file; if they proceed,
      // the buffer will automatically be renamed (by internals) after it is saved
      (if (!editor.buffers.exists(_.file == file)) Future.success(true)
      else editor.miniReadYN(s"A buffer is visiting '$path'; proceed?")) onSuccess {
        case false => editor.emitStatus("Canceled.")
        case true =>
          // require confirmation if the target file already exists
          (if (!file.exists) Future.success(true)
          else editor.miniReadYN(s"File '$path' exists; overwrite?")) onSuccess {
            case false => editor.emitStatus("Canceled.")
            case true =>
              buffer.saveTo(file)
              editor.emitStatus(s"Wrote: ${buffer.file.getAbsolutePath}")
          }
      }
    }
  }

  //
  // EDITOR FNS

  @Fn("""Offers to save any unsaved buffers, then kills this editor.""")
  def saveBuffersKillEditor () {
    // TODO: save buffers!
    editor.exit(0)
  }

  //
  // META FNS

  @Fn("Reads fn name then invokes it.")
  def executeExtendedCommand () {
    editor.miniRead("M-x", "", disp.completeFn) onSuccess { fn =>
      if (!disp.invoke(fn)) editor.emitStatus(s"Unknown fn: $fn")
    }
  }
}
