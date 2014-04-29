//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import java.io.File
import reactual.{Future, Promise}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.util.Chars

/** Configuration for [[EditingMode]]. */
object EditingConfig extends Config.Defs {
  // nada for now
}

/** A base class for all modes that support interactive text editing. This mode extends
  * [[ReadingMode]] with fns that actually change the contents of the buffer.
  * Most major modes will inherit from this mode.
  */
abstract class EditingMode (env :Env) extends ReadingMode(env) {
  import Chars._
  import EditingConfig._
  import EditorConfig._

  override def defaultFn :Option[String] = Some("self-insert-command")

  override def configDefs = EditingConfig :: super.configDefs
  override def keymap = super.keymap ++ Seq(
    // character editing commands
    "BS"    -> "delete-backward-char", // TODO: make this delete back to mark (if set)
    "DEL"   -> "delete-forward-char", // ...forward to mark (if set)
    "C-d"   -> "delete-forward-char", // this should be delete-char and ignore mark

    "ENTER"   -> "newline",
    "S-ENTER" -> "newline",
    "TAB"     -> "indent-for-tab-command",
    // TODO: open-line, split-line, ...

    "C-t"     -> "transpose-chars",
    "C-x C-u" -> "upcase-region",
    "C-x C-l" -> "downcase-region",
    "M-u"     -> "upcase-word",
    "M-l"     -> "downcase-word",
    "M-c"     -> "capitalize-word",

    "C-M-l"   -> "sort-paragraph",
    // NONE   -> "sort-lines",

    // killing and yanking commands
    "C-w"     -> "kill-region",
    "C-M-w"   -> "append-next-kill",
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

    // replacing commands
    // TODO: "replace-string", "replace-regepx"

    // buffer commands
    "C-x C-s" -> "save-buffer",
    "C-x C-w" -> "write-file"
  )

  /** Deletes the region `[from, to)` from the buffer and adds it to the kill-ring. If `to` is
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
        if (isBackwardKill(disp.curFn)) config(killRing) prepend region
        else                            config(killRing) append  region
      }
      // otherwise create a new kill ring entry
      else config(killRing) add region
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

  /** Converts the characters in `[from, to)` to upper case. If `to` is earlier than `from` the
    * arguments are automatically swapped.
    * @return the end of the upcased region (the larger of `from` and `to`).
    */
  def upcase (from :Loc, to :Loc) :Loc = buffer.transform(from, to, Character.toUpperCase)

  /** Converts the characters in `[from, to)` to lower case. If `to` is earlier than `from` the
    * arguments are automatically swapped.
    * @return the end of the upcased region (the larger of `from` and `to`).
    */
  def downcase (from :Loc, to :Loc) :Loc = buffer.transform(from, to, Character.toLowerCase)

  /** Sorts the lines in the region `[start, end)`. */
  def sortLinesIn (start :Loc, end :Loc) {
    val r = Region(start, buffer.backward(end, 1))
    val lines = buffer.region(r)
    val sorted = lines.sorted(LineV.ordering)
    if (lines != sorted) buffer.replace(r, sorted)
    else editor.popStatus("Region already sorted.")
  }

  //
  // CHARACTER EDITING FNS

  @Fn("Inserts the character you typed.")
  def selfInsertCommand (typed :String) {
    // insert the typed character at the point
    val p = view.point()
    view.buffer.insert(p, typed, Styles.None)
    // move the point to the right by the appropriate amount
    view.point() = p + (0, typed.length)
  }

  @Fn("Deletes the character immediately previous to the point.")
  def deleteBackwardChar () {
    val vp = view.point()
    val prev = buffer.backward(vp, 1)
    if (prev == vp) editor.emitStatus("Beginning of buffer.")
    else buffer.delete(prev, vp)
    // move the point back one space (TODO: should this be necessary?; I get the idea that buffer
    // deletions prior to the point in Emacs just cause the point to move)
    view.point() = prev
  }

  @Fn("Deletes the character at the point.")
  def deleteForwardChar () {
    val del = view.point()
    val next = buffer.forward(del, 1)
    if (del == next) editor.emitStatus("End of buffer.")
    else buffer.delete(del, next)
  }

  @Fn("""Inserts a newline at the point.
         Characters after the point on the current line wil be moved to a new line.""")
  def newline () {
    val p = view.point()
    buffer.split(p)
    view.point() = p.nextStart
  }

  @Fn("Indents the current line or region, or inserts a tab, as appropriate.")
  def indentForTabCommand () {
    editor.emitStatus("TODO: tabs!")
  }

  @Fn("""Swaps the character at the point with the character preceding it, and moves the point
         forward one character. If the point is at the start of a line, the character will be
         'transposed' with the newline preceding it, effectively moving the character to the
         previous line. If the point is past the end of a line, the character at the end of the
         line will be transposed with the preceding character.""")
  def transposeChars () {
    // the passages below are a bit twisty, but it (mostly) mimics what emacs does
    val p = view.point()
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
        case 0 =>  editor.popStatus("Nothing to transpose.")
        // otherwise pull the last character of the previous line into this one
        case len =>
          val last = Loc(prev, len-1)
          val deleted = buffer.delete(last, 1)
          buffer.insert(buffer.lineStart(p), deleted)
          view.point() = tp.nextC
      }
    }
    // otherwise we have a normal transpose: swap the char under the point with the prev char
    else {
      val swap = tp.prevC
      buffer.replace(swap, 2, new Line(Array(buffer charAt tp, buffer charAt swap),
                                       Array(buffer stylesAt tp, buffer stylesAt swap)))
      view.point() = tp.nextC
    }
  }

  @Fn("""Converts the region to upper case, moving the point to the end of the region.""")
  def upcaseRegion () = withRegion(upcase)

  @Fn("""Converts the region to lower case, moving the point to the end of the region.""")
  def downcaseRegion () = withRegion(downcase)

  @Fn("""Converts the following word to upper case, moving the point to the end of the word.""")
  def upcaseWord () {
    view.point() = upcase(view.point(), forwardWord(view.point()))
  }

  @Fn("""Converts the following word to lower case, moving the point to the end of the word.""")
  def downcaseWord () {
    view.point() = downcase(view.point(), forwardWord(view.point()))
  }

  @Fn("""Capitalizes the word at or following the point, moving the point to the end of the word.
         This gives the word a first character in upper case and the rest in lower case.""")
  def capitalizeWord () {
    val first = buffer.scanForward(isWord, view.point())
    val start = buffer.forward(first, 1)
    val until = buffer.scanForward(isNotWord, start)
    upcase(first, start)
    downcase(start, until)
    view.point() = until
  }

  @Fn("""Alphabetically sorts the lines in the paragraph under the point or the previous paragraph
         if the point is on an empty line.""")
  def sortParagraph () :Unit = withParagraph(sortLinesIn)

  @Fn("""Alphabetically sorts the lines in the current region.""")
  def sortLines () :Unit = withRegion(sortLinesIn)

  //
  // KILLING AND YANKING FNS

  @Fn("""Kills the text between the point and the mark. This removes the text from the buffer and
         adds it to the kill ring. The point and mark are moved to the start of the killed
         region.""")
  def killRegion () = withRegion { (start, end) =>
    view.point() = kill(start, end)
    buffer.mark = view.point()
  }

  @Fn("""Causes the following command, if it kills, to append to the previous kill rather than
         creating a new kill-ring entry.""")
  def appendNextKill () {
    editor.emitStatus("If next command is a kill, it will append.")
    // kill() will note that the prevFn is append-next-kill and append appropriately
  }

  @Fn("""Kills the rest of the current line, adding it to the kill ring. If the point is at the end
         of the line, the newline is killed instead.""")
  def killLine () {
    val p = view.point()
    val eol = buffer.lineEnd(p)
    // if we're at the end of the line, kill to the first line of the next char (the newline)
    // otherwise kill to the end of the line
    kill(p, if (p == eol) buffer.forward(p, 1) else eol)
  }

  @Fn("""Kills the entire current line.""")
  def killWholeLine () {
    val p = view.point()
    view.point() = kill(buffer.lineStart(p), buffer.forward(buffer.lineEnd(p), 1))
  }

  @Fn("""Kills characters forward until encountering the end of a word.""")
  def killWord () {
    kill(view.point(), forwardWord(view.point()))
  }

  @Fn("""Kills characters backward until encountering the beginning of a word.""")
  def backwardKillWord () {
    view.point() = kill(backwardWord(view.point()), view.point())
  }

  @Fn("""Reinserts the most recently killed text. The mark is set to the point and the point is
         moved to the end if the inserted text.""")
  def yank () {
    config(killRing).entry(0) match {
      case None => editor.popStatus("Kill ring is empty.")
      case Some(region) =>
        buffer.mark = view.point()
        view.point() = buffer.insert(view.point(), region)
    }
  }

  @Fn("""Replaces the just-yanked stretch of killed text with a different stretch.""")
  def yankPop () {
    if (!yanks(disp.prevFn)) editor.popStatus(s"Previous command was not a yank (${disp.prevFn}).")
    else {
      yankCount = if (disp.prevFn == "yank-pop") yankCount + 1 else 1
      config(killRing).entry(yankCount) match {
        case None => editor.popStatus("Kill ring is empty.")
        case Some(region) =>
          // since the last command was a yank, the mark must be set
          val mark = buffer.mark.get
          buffer.mark = view.point() lesser mark
          view.point() = buffer.replace(view.point(), mark, region)
      }
    }
  }
  private var yankCount = 0
  private val yanks = Set("yank", "yank-pop")

  //
  // UNDO FNS

  @Fn("Undoes the last change to the buffer.")
  def undo () = buffer.undoer.undo() match {
    case None    => editor.popStatus("Nothing to undo.")
    case Some(p) => view.point() = p
  }

  @Fn("Redoes the last undone to the buffer.")
  def redo () = buffer.undoer.redo() match {
    case None    => editor.popStatus("Nothing to redo.")
    case Some(p) => view.point() = p
  }

  //
  // REPLACING FNS

  // TODO

  //
  // BUFFER FNS

  @Fn("""Saves current buffer to the currently visited file, if modified.""")
  def saveBuffer () {
    if (!buffer.dirty) editor.emitStatus("No changes need to be saved.")
    else {
      // TODO: all sorts of checks; has the file changed (out from under us) since we loaded it?
      // what else does emacs do?
      buffer.save()
      editor.popStatus(s"Wrote: ${buffer.name}", s"into: ${buffer.dir.getAbsolutePath}")
    }
  }

  @Fn("""Saves the buffer to a filename read from the minibuffer. This makes the buffer visit that
         file. If you specify just a directory, the buffer will be saved to its current filename
         in the specified directory.""")
  def writeFile () {
    val bufwd = buffer.dir.getAbsolutePath + File.separator
    editor.miniRead("Write file:", bufwd, config(fileHistory), Completer.file) onSuccess { lfile =>
      val file = lfile.getCanonicalFile
      // require confirmation if another buffer is visiting the specified file; if they proceed,
      // the buffer will automatically be renamed (by internals) after it is saved
      (if (!editor.buffers.exists(_.file == file)) Future.success(true)
      else editor.miniReadYN(s"A buffer is visiting '$lfile'; proceed?")) onSuccess {
        case false => editor.popStatus("Canceled.")
        case true =>
          // require confirmation if the target file already exists
          (if (!file.exists) Future.success(true)
          else editor.miniReadYN(s"File '$lfile' exists; overwrite?")) onSuccess {
            case false => editor.popStatus("Canceled.")
            case true =>
              buffer.saveTo(file)
              editor.popStatus(s"Wrote: ${buffer.name}", s"into: ${buffer.dir.getAbsolutePath}")
          }
      }
    }
  }
}
