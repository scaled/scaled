//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._
import scaled.util.{Chars, Filler}

/** Configuration for [[EditingMode]]. */
object EditingConfig extends Config.Defs {

  @Var("""The column at which lines are wrapped (by `fill-` fns, and automatically during typing
          if `auto-fill` is true). Set to -1 to use the current window width as the fill column.""")
  val fillColumn = key(-1)

  @Var("""If true, inserting a character at or beyond `fill-column` automatically breaks the line
          at the previous space.""")
  val autoFill = key(false)
}

/** A base class for all modes that support interactive text editing. This mode extends
  * [[ReadingMode]] with fns that actually change the contents of the buffer.
  * Most major modes will inherit from this mode.
  */
abstract class EditingMode (env :Env) extends ReadingMode(env) {
  import Chars._
  import Workspace._
  import EditorConfig._

  override def defaultFn :Option[String] = Some("self-insert-command")

  override def configDefs = EditingConfig :: super.configDefs
  override def keymap = super.keymap.
    // character editing commands
    bind("delete-backward-char", "BS"). // TODO: make this delete back to mark (if set)
    bind("delete-forward-char",  "DEL", "C-d"). // ...forward to mark (if set)
    // TODO: C-d should be delete-char and ignore mark

    bind("newline",                "ENTER", "S-ENTER").
    bind("indent-for-tab-command", "TAB").
    // TODO: open-line, split-line, ...

    bind("transpose-chars", "C-t").
    bind("upcase-region",   "C-x C-u").
    bind("downcase-region", "C-x C-l").
    bind("upcase-word",     "M-u").
    bind("downcase-word",   "M-l").
    bind("capitalize-word", "M-c").

    bind("sort-paragraph", "C-M-l").
    //   "sort-lines", "no binding",
    //   "reverse-region", "no binding",
    bind("fill-paragraph", "M-q").

    // killing and yanking commands
    bind("kill-region",        "C-w").
    bind("append-next-kill",   "C-M-w").
    bind("kill-line",          "C-k").
    bind("kill-whole-line",    "C-S-BS").
    bind("kill-word",          "M-d").
    bind("backward-kill-word", "M-DEL").
    bind("backward-kill-word", "C-BS").
    bind("join-line",          "M-BS").
    // bind("zap-to-char", "M-z"),
    // bind("kill-sentence", "M-k"), // do we want?
    // bind("backward-kill-sentence", "C-x DEL"), // do we want?
    // bind("kill-balanced-sexp", "C-M-k"), // do we want?

    bind("yank",     "C-y").
    bind("yank-pop", "M-y").

    // undo commands
    bind("undo", "C-/", "C-x u", "C-_").
    bind("redo", "C-\\", "C-x r").
    // TEMP: until we sort out ctrl'd shifted keys
    bind("undo", "C-S--").

    // replacing commands
    // TODO: "replace-string", "replace-regepx"

    // buffer commands
    bind("save-buffer", "C-x C-s").
    bind("write-file",  "C-x C-w");

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

  /** Turns start and end into a region, with the caveat that if end is the 0th col on last line
    * of the region, the end is backed up to the last character of the previous line. This avoids
    * including a trailing blank line in fns that operate on a region.
    */
  def trimRegion (start :Loc, end :Loc) :Region = Region(start, trimEnd(end))

  /** "Trims" the end of a region such that if end is the 0th col on last line of the region, the
    * end is backed up to the last character of the previous line. This avoids including a trailing
    * blank line in fns that operate on a region.
    */
  def trimEnd (end :Loc) :Loc = if (end.col == 0) buffer.backward(end, 1) else end

  /** Sorts the lines in the region `[start, end)`. */
  def sortLinesIn (start :Loc, end :Loc) {
    val r = trimRegion(start, end)
    val lines = buffer.region(r)
    val sorted = lines.sorted(LineV.ordering)
    if (lines != sorted) buffer.replace(r, sorted)
    else window.popStatus("Region already sorted.")
  }

  /** Reverses the lines in the region `[start, end)`. */
  def reverseLinesIn (start :Loc, end :Loc) {
    val r = trimRegion(start, end)
    buffer.replace(r, buffer.region(r).reverse)
  }

  override def fillColumn :Int = config(EditingConfig.fillColumn) match {
    case -1 => super.fillColumn
    case cc => cc
  }

  /** Refills the lines in the region `[start, end)`, wrapping them at `fill-column`. */
  def refillLinesIn (start :Loc, end :Loc) {
    val r = trimRegion(start, end)
    val orig = buffer.region(r)
    val filler = new Filler(fillColumn)
    orig foreach(filler.append)
    val filled = filler.toLines
    if (filled != orig) buffer.replace(r, filled)
    else window.emitStatus("Region already filled.")
  }

  /** Returns true if we should auto-fill, false if not. The default implementation checks that
    * auto-fill is enabled, and that the point is at or beyond the fill column.
    * @param p the current point.
    */
  def shouldAutoFill (p :Loc) :Boolean = config(EditingConfig.autoFill) && (p.col >= fillColumn)

  /** Computes the location at which we should break the line that contains `p` for auto-fill.
    * The default implementation seeks backward from `p` to the first space.
    * @return the location at which to break the line, or `Loc.None` to cancel the break.
    */
  def computeAutoBreak (p :Loc) :Loc =
    buffer.findBackward(Matcher.exact(" "), p, p.atCol(0)) match {
      case Loc.None => Loc.None
      case space => space.nextC // break *after* the space
    }

  /** Auto-breaks a line at `at`. */
  def autoBreak (at :Loc) {
    buffer.split(at)
    // trim whitespace immediately preceeding `at` (but only up to the start of the line)
    val from = buffer.scanBackWhile(Chars.isWhitespace, at, at.atCol(0))
    if (from < at) buffer.delete(from, at)
  }

  //
  // CHARACTER EDITING FNS

  @Fn("Inserts the character you typed.")
  def selfInsertCommand (typed :String) {
    // if auto-fill is activated and we're about to insert beyond the fill column, first break
    // the current line at the previous space
    val p = view.point()
    if (shouldAutoFill(p)) {
      val at = computeAutoBreak(p)
      if (at != Loc.None) autoBreak(at)
    }
    // re-read view.point() here because breakForAutofill() may have changed it
    val np = view.point()
    // insert the typed character at the point
    if (typed.length != 1) view.buffer.insert(np, Line(typed))
    else view.buffer.insert(np, typed.charAt(0), Syntax.Default)
  }

  @Fn("Deletes the character immediately previous to the point.")
  def deleteBackwardChar () {
    val vp = view.point()
    val prev = buffer.backward(vp, 1)
    if (prev == vp) window.emitStatus("Beginning of buffer.")
    else buffer.delete(prev, vp)
  }

  @Fn("Deletes the character at the point.")
  def deleteForwardChar () {
    val del = view.point()
    val next = buffer.forward(del, 1)
    if (del == next) window.emitStatus("End of buffer.")
    else buffer.delete(del, next)
  }

  @Fn("""Inserts a newline at the point.
         Characters after the point on the current line wil be moved to a new line.""")
  def newline () {
    buffer.split(view.point())
  }

  @Fn("Indents the current line or region, or inserts a tab, as appropriate.")
  def indentForTabCommand () {
    // TODO: I suppose we'll eventually have to support real tabs... sigh
    buffer.insert(view.point(), Line.fromText("  "))
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
    if (tp == Loc.Zero) window.emitStatus("Beginning of buffer.")
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
        case 0 =>  window.popStatus("Nothing to transpose.")
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
                                       Array(buffer syntaxAt tp, buffer syntaxAt swap)))
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

  @Fn("Reverses the order of lines in the current region.")
  def reverseRegion () :Unit = withRegion(reverseLinesIn)

  @Fn("""Fills the paragraph at or before the point. All lines in the paragraph will be
         concatenated and then rewrapped into lines no longer than `fill-column` characters in
         length.""")
  def fillParagraph () :Unit = withParagraph(refillLinesIn)

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
    window.emitStatus("If next command is a kill, it will append.")
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
    kill(backwardWord(view.point()), view.point())
  }

  @Fn("""Joins this line to the previous line. Deletes from the first whitespace character at the
         end of the previous line to the last whitespace character at the start of the current
         line, including the intervening line break.""")
  def joinLine () {
    val cloc = view.point() ; val cstart = buffer.lineStart(cloc)
    if (cstart > Loc.Zero) {
      val ploc = cloc.prevStart
      // find the first non-whitespace character on this line
      val cjoin = buffer.scanForward(isNotWhitespace, cstart, buffer.lineEnd(cloc))
      // find the last non-whitespace character on the previous line
      val pjoin = buffer.scanBackWhile(isWhitespace, buffer.lineEnd(ploc), buffer.lineStart(ploc))
      buffer.delete(pjoin, cjoin)
    } // otherwise we're on the first line so there's nothing to which to join
  }

  @Fn("""Reinserts the most recently killed text. The mark is set to the point and the point is
         moved to the end if the inserted text.""")
  def yank () {
    editor.killRing.entry(0) match {
      case None => window.popStatus("Kill ring is empty.")
      case Some(region) =>
        buffer.mark = view.point()
        buffer.insert(view.point(), region)
    }
  }

  @Fn("""Replaces the just-yanked stretch of killed text with a different stretch.""")
  def yankPop () {
    if (!yanks(disp.prevFn)) window.popStatus(s"Previous command was not a yank (${disp.prevFn}).")
    else {
      yankCount = if (disp.prevFn == "yank-pop") yankCount + 1 else 1
      editor.killRing.entry(yankCount) match {
        case None => window.popStatus("Kill ring is empty.")
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
    case None    => window.popStatus("Nothing to undo.")
    case Some(p) => view.point() = p
  }

  @Fn("Redoes the last undone to the buffer.")
  def redo () = buffer.undoer.redo() match {
    case None    => window.popStatus("Nothing to redo.")
    case Some(p) => view.point() = p
  }

  //
  // REPLACING FNS

  /** Handles querying the user for the FROM and TO string, offering and using defaults, etc.
    * If search terms are obtained, `repFn` is invoked with them.
    */
  def getReplaceArgs (prefix :String, repFn :(String, String) => Unit) {
    val history = replaceHistory(wspace)
    val defrep = history.entries match {
      case 0 => None
      case 1 => Some(Line.toText(history.entry(0).get) -> "")
      case n => Some(Line.toText(history.entry(1).get) -> Line.toText(history.entry(0).get))
    }
    val defprompt = defrep match {
      case Some((from, to)) => s" default ($from -> $to)"
      case _ => ""
    }
    window.mini.read(s"$prefix$defprompt:", "", history, Completer.none) onSuccess { from =>
      if (from == "") defrep match {
        case Some((from, to)) => repFn(from, to)
        case                _ => window.emitStatus("Aborted")
      }
      else {
        val prompt = s"$prefix '$from' with:"
        window.mini.read(prompt, "", history, Completer.none) onSuccess { to =>
          // normally MiniReadMode won't add a blank response to the history, but in the case of
          // a blank 'to' replacement, we do want to add it to the history
          if (to == "") history.add(Line.fromText(to))
          // now invoke the replacement fn
          repFn(from, to)
        }
      }
    }
  }

  /** Non-interactively replaces all matches of `search` with `to`, starting at `start`.
    * The number of replacements will be reported on completion.
    * @param preCount the number of replacements already performed prior to this call.
    */
  def replaceAll (search :Search, to :Seq[LineV], start :Loc, preCount :Int = 0) {
    @inline @tailrec def loop (loc :Loc, count :Int) :Int = {
      val next = search.findForward(loc)
      if (next != Loc.None) loop(search.replace(buffer, next, to), count+1)
      else count
    }
    val total = loop(start, preCount)
    window.emitStatus(s"Replaced $total occurrence(s).")
  }

  /** Processes a query replace of `fromM` with `to` from `start`. If no more matches exist, the
    * query replace is considered done and success is reported. Otherwise the next match is sought
    * and the user queried as to whether it should be replaced, and then the query replace is
    * repeated following that match.
    */
  def queryReplace (search :Search, to :Seq[LineV], start :Loc) {
    val prompt = s"Query replacing '${search.show}' with '${Line.toText(to)}': (C-h for help)"
    val opts = Seq(
      "y"  -> "replaces one match",
      "n"  -> "skips to the next match",
      " "  -> "replaces one match",
      "BS" -> "skips to the next match",
      "."  -> "replace one match and exit",
      "!"  -> "replace all remaining matches",
      "q"  -> "exit, skipping all remaining matches"
    )
    def done (count :Int) :Unit = window.emitStatus(s"Replaced $count occurrence(s).")
    def clear (loc :Loc) = buffer.removeStyle(activeMatchStyle, loc, search.matchEnd(loc))
    def loop (from :Loc, count :Int) {
      val next = search.findForward(from)
      if (next == Loc.None) done(count)
      else {
        buffer.addStyle(activeMatchStyle, next, search.matchEnd(next))
        view.point() = next
        val nextEnd = search.matchEnd(next)
        window.statusMini.readOpt(prompt, opts).onComplete(_ => clear(next)).onSuccess(_ match {
          case "y"|" "  => loop(search.replace(buffer, next, to), count+1)
          case "n"|"BS" => loop(nextEnd, count)
          case "q"      => done(count)
          case "!"      => replaceAll(search, to, next, count)
          case "."      => search.replace(buffer, next, to); done(count+1)
        })
      }
    }
    loop(start, 0)
  }

  @Fn("""Queries the user for a FROM and TO string. Replaces all instances of FROM with TO
         from the point to the end of the buffer.""")
  def replaceString () {
    getReplaceArgs("Replace string", (from, to) => {
      // TODO: provide a way to force exact matching on lower-case strings? or just have 'em
      // use a regexp for that?

      // TODO: transient mark mode and replacing in the region; that's super useful, so maybe
      // this alone is worth the trouble of emulating transient mark mode...
      val search = Search(buffer, view.point(), buffer.end, Matcher.on(from))
      replaceAll(search, Line.fromText(to), search.min)
    })
  }

  @Fn("""Queries the user for a FROM regexp and TO string. Replaces all instances of FROM with TO
         from the point to the end of the buffer.""")
  def replaceRegexp () {
    getReplaceArgs("Replace regexp", (from, to) => {
      // TODO: transient mark mode and replacing in the region
      val search = Search(buffer, view.point(), buffer.end, Matcher.regexp(from))
      replaceAll(search, Line.fromText(to), search.min)
    })
  }

  @Fn("""Queries the user for a FROM and TO string. Replaces all instances of FROM with TO
         from the point to the end of the buffer with interactive confirmation.""")
  def queryReplace () {
    getReplaceArgs("Query replace", (from, to) => {
      val search = Search(buffer, view.point(), buffer.end, Matcher.on(from))
      queryReplace(search, Line.fromText(to), search.min)
    })
  }

  // TODO: query-replace-regexp

  //
  // BUFFER FNS

  @Fn("""Saves current buffer to the currently visited file, if modified.""")
  def saveBuffer () {
    if (!buffer.dirty) window.emitStatus("No changes need to be saved.")
    else {
      // TODO: all sorts of checks; has the file changed (out from under us) since we loaded it?
      // what else does emacs do?
      buffer.save()
      window.emitStatus(s"Wrote: ${buffer.name}")
    }
  }

  @Fn("""Saves the buffer to a filename read from the minibuffer. This makes the buffer visit that
         file. If you specify just a directory, the buffer will be saved to its current filename
         in the specified directory.""")
  def writeFile () {
    val bufwd = buffer.store.parent
    window.mini.read("Write file:", bufwd, fileHistory(wspace), Completer.file) onSuccess { store =>
      // require confirmation if another buffer is visiting the specified file; if they proceed,
      // the buffer will automatically be renamed (by internals) after it is saved
      (if (!wspace.buffers.exists(_.store == store)) Future.success(true)
       else window.mini.readYN(s"A buffer is visiting '${store.name}'; proceed?")) onSuccess {
        case false => window.popStatus("Canceled.")
        case true =>
          // require confirmation if the target file already exists
          (if (!store.exists) Future.success(true)
           else window.mini.readYN(s"File '$store' exists; overwrite?")) onSuccess {
             case false => window.popStatus("Canceled.")
             case true =>
               buffer.saveTo(store)
               window.popStatus(s"Wrote: ${buffer.name}", s"into: ${store.parent}")
           }
       }
    }
  }
}
