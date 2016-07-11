//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._
import scaled.util.{Chars, Paragrapher}

/** Configuration for [[ReadingMode]]. */
object ReadingConfig extends Config.Defs {

  // nada for now
}

/** A base class for major modes which allow text to be read but not edited. Motion and other
  * non-editing commands are defined here, but editing commands are deferred to [[EditingMode]]
  * which extends this one.
  */
abstract class ReadingMode (env :Env) extends MajorMode(env) {
  import Chars._
  import ReadingConfig._

  override def missedFn  :Option[String] = Some("unknown-command")

  override def configDefs = ReadingConfig :: super.configDefs
  override def keymap = super.keymap.
    // mark manipulation commands
    bind("set-mark-command",        "C-SPACE", "C-@"). // needs to be C-S-2? meh.
    bind("exchange-point-and-mark", "C-x C-x").

    // the one non-destructive kill command
    bind("kill-ring-save", "M-w").

    // searching commands
    bind("isearch-forward",  "C-s").
    bind("isearch-backward", "C-r").

    // motion commands
    bind("backward-char", "C-b", "LEFT").
    bind("forward-char",  "C-f", "RIGHT").

    bind("backward-word", "M-b", "C-LEFT").
    bind("forward-word",  "M-f", "C-RIGHT").

    bind("move-beginning-of-line", "C-a", "HOME").
    bind("move-end-of-line",       "C-e", "END").

    bind("previous-line", "C-p", "UP").
    bind("next-line",     "C-n", "DOWN").

    bind("previous-paragraph", "C-UP").
    bind("next-paragraph",     "C-DOWN").

    bind("beginning-of-buffer", "M-<", "C-HOME", "BEGIN").
    bind("end-of-buffer",       "M->", "C-END").
    // TEMP: until we sort out meta'd shifted keys
    bind("beginning-of-buffer", "M-S-,").
    bind("end-of-buffer",       "M-S-.").

    bind("goto-line",   "M-g").
    bind("goto-offset", "M-S-g").

    // view commands (scrolling, etc.)
    bind("scroll-up",        "S-UP"). // TODO: extend-mark-backward-line
    bind("scroll-down",      "S-DOWN"). // TODO: extend-mark-forward-line
    bind("scroll-up-page",   "M-v").
    bind("scroll-down-page", "C-v").
    bind("scroll-up-page",   "PGUP").
    bind("scroll-down-page", "PGDN").

    bind("recenter", "C-l").

    // help commands
    bind("show-tags",      "M-A-t").
    bind("show-line-tags", "M-C-t")

  /** Seeks forward to the end a word. Moves forward from `p` until at least one word char is seen,
    * and then keeps going until a non-word char is seen (or the end of the buffer is reached), and
    * that point is returned.
    */
  def forwardWord (p :Loc) :Loc = buffer.scanForward(isNotWord, buffer.scanForward(isWord, p))

  /** Seeks backward to the start of a word. Moves backward from `p` until at least one word char is
    * seen (whether `p` is a word char is not relevant), and then keeps going until a non-word char
    * is seen (or the start of the buffer is reached), and returns the loc of the last seen word
    * char.
    */
  def backwardWord (p :Loc) :Loc = {
    val firstWordC = buffer.scanBackward(isWord, p)
    // we may have hit the beginning of the buffer looking for a word char; if so, cope
    if (firstWordC == buffer.start) buffer.start
    else buffer.scanBackWhile(isWord, firstWordC)
  }

  /** Invokes `fn` with the start and end of the current region. If the mark is not set, a status
    * message is emitted that indicates that the current region is not set and `fn` is not
    * invoked. */
  def withRegion (fn :(Loc, Loc) => Unit) :Unit = buffer.mark match {
    case None => window.popStatus("The mark is not set now, so there is no region.")
    case Some(mp) => val p = view.point() ; if (mp < p) fn(mp, p) else fn(p, mp)
  }

  /** Invokes `fn` with the start and end of the current region. If the mark is not set, a fake
    * region which starts at the beginning of the line at the point and continues to the beginning
    * of the next line is used. */
  def withRegionOrLine (fn :(Loc, Loc) => Unit) :Unit = {
    val p = view.point() ; val mp = buffer.mark || p.nextStart
    if (mp < p) fn(mp, p) else fn(p, mp)
  }

  /** Creates a paragrapher for the specified syntax. */
  def mkParagrapher (syntax :Syntax) = new Paragrapher(syntax, buffer)

  /** Invokes `fn` with the start and end of the paragraph under the point. A paragraph is a
    * series of lines delimited on both ends by blank lines or an end of the buffer. If the point
    * is on a blank line, the previous paragraph will be sought. If no previous paragraph can be
    * found, the paragraph following the point is sought. If no paragraph can still be found
    * (the buffer is completely empty), an error message is reported to the user and `fn` is not
    * invoked.
    */
  def withParagraph (fn :(Loc, Loc) => Unit) :Unit = {
    // use the syntax of the first non-whitespace char on the line at the point to determine our
    // "mode" when identifying paragraph boundaries
    val p = view.point()
    val syn = buffer.line(p).indexOf(isNotWhitespace) match {
      case -1 => Syntax.Default
      case ii => buffer.line(p).syntaxAt(ii)
    }
    mkParagrapher(syn).paragraphAt(p) match {
      case None => window.popStatus("Unable to find a paragraph.")
      case Some(r) => fn(r.start, r.end)
    }
  }

  /** Returns the fill column. By default this is based on the width of the view, but all editing
    * modes specialize this and allow the user to customize the value. */
  def fillColumn :Int = view.width()-1

  //
  // KILLING AND YANKING FNS

  @Fn("""Saves the text between the point and the mark as if killed, but doesn't kill it.
         The point and mark remain unchanged.""")
  def killRingSave () = withRegion { (start, end) =>
    editor.killRing add buffer.region(start, end)
    window.emitStatus("Region added to kill ring.")
  }

  //
  // SEARCHING FNS

  @Fn("""Searches incrementally forward. As you type characters, they add to the search string
         and are immediately sought. The following key bindings control the search:

         Type DEL to cancel last input item from end of search string.
         Type RET to exit, leaving point at location found.
         Type C-j to match end of line.
         Type C-s to search again forward, C-r to search again backward.
         Type C-w to yank next word or character in buffer onto the end of the search string.
         Type M-s C-e to yank rest of line onto end of search string and search for it.
         Type C-y to yank the last string of killed text.
         Type M-y to replace string just yanked into search prompt with string killed before it.
         Type C-q to quote control character to search for it.

         C-g while searching or when search has failed cancels input back to what has been found
           successfully.
         C-g when search is successful aborts and moves point to starting point.
         """)
  def isearchForward () {
    window.statusMini("isearch", Promise[Boolean](), view, disp, "forward")
  }

  @Fn("Searches incrementally backward. See the command isearch-forward for more info.")
  def isearchBackward () {
    window.statusMini("isearch", Promise[Boolean](), view, disp, "backward")
  }

  //
  // MARK MANIPULATION FNS

  @Fn("Sets the mark to the current point.")
  def setMarkCommand () {
    // TODO: push old mark onto local (buffer?) and global mark ring?
    buffer.mark = view.point()
    window.emitStatus("Mark set.")
  }

  @Fn("Sets the mark to the current point and moves the point to the previous mark.")
  def exchangePointAndMark () {
    buffer.mark match {
      case Some(m) =>
        buffer.mark = view.point()
        view.point() = m
      case None =>
        window.popStatus("No mark set in this buffer.")
    }
  }

  //
  // MOTION FNS

  @Fn("Moves the point forward one character.")
  def forwardChar () {
    val old = view.point()
    // if we're at the end of the current line, move to the next line
    view.point() = buffer.forward(old, 1)
    // if the point didn't change, that means we tried to move past the end of the buffer
    if (old == view.point()) window.emitStatus("End of buffer.")
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val old = view.point()
    view.point() = buffer.backward(old, 1)
    if (old == view.point()) window.emitStatus("Beginning of buffer.")
  }

  @Fn("Moves the point forward one word.")
  def forwardWord () {
    view.point() = forwardWord(view.point())
  }

  @Fn("Moves the point backward one word.")
  def backwardWord () {
    view.point() = backwardWord(view.point())
  }

  // track the current column; we'll use that to track our "desired column" in situations where
  // we move vertically through lines that are not wide enough to accommodate our desired column
  // and then back into lines that are
  private[this] var desiredColumn = 0
  view.point.onValue { p => desiredColumn = p.col }

  @Fn("Moves the point down one line.")
  def nextLine () {
    val old = view.point()
    // attempt to move into our "desired column" in the next row; in most cases the desired column
    // will be equal to the current column, but when moving from a long line, through some short
    // ones and back into a long one, a longer desired column may be preserved
    val oldDesiredColumn = desiredColumn
    view.point() = Loc(old.row+1, oldDesiredColumn)
    if (old == view.point()) window.emitStatus("End of buffer.") // TODO: with beep?
    // if the line to which we moved was short, desiredColumn will have been overwritten with
    // a too small value; so we restore it now
    desiredColumn = oldDesiredColumn
  }

  @Fn("Moves the point up one line.")
  def previousLine () {
    val old = view.point()
    // see nextLine() for a description of what we're doing here with desiredColumn
    val oldDesiredColumn = desiredColumn
    view.point() = Loc(old.row-1, oldDesiredColumn)
    if (old == view.point()) window.emitStatus("Beginning of buffer.") // TODO: with beep?
    desiredColumn = oldDesiredColumn
  }

  @Fn("""Moves to the next paragraph. Paragraphs are currently delimited by blank lines.
         TODO: make this more emacs-like?""")
  def nextParagraph () {
    @tailrec def seek (row :Int, seenNonBlank :Boolean) :Loc = {
      if (row >= buffer.lines.size) Loc(row, 0)
      else {
        val line = buffer.line(row)
        val isBlank = line.firstNonWS == line.length
        if (isBlank && seenNonBlank) Loc(row, 0)
        else seek(row+1, seenNonBlank || !isBlank)
      }
    }
    view.point() = seek(view.point().row, false)
  }

  @Fn("""Moves to the previous paragraph. Paragraphs are currently delimited by blank lines.
         TODO: make this more emacs-like?""")
  def previousParagraph () {
    @tailrec def seek (row :Int, seenNonBlank :Boolean) :Loc = {
      if (row <= 0) Loc(0, 0)
      else {
        val line = buffer.line(row)
        val isBlank = line.firstNonWS == line.length
        if (isBlank && seenNonBlank) Loc(row, 0)
        else seek(row-1, seenNonBlank || !isBlank)
      }
    }
    view.point() = seek(view.point().row, false)
  }

  @Fn("Moves the point to the beginning of the line.")
  def moveBeginningOfLine () = view.point() = view.point().atCol(0)

  @Fn("Moves the point to the end of the line.")
  def moveEndOfLine () = view.point() = view.point().atCol(buffer.line(view.point()).length)

  @Fn("Moves the point to the beginning of the buffer.")
  def beginningOfBuffer () = view.point() = buffer.start

  @Fn("Moves the point to the end of the buffer.")
  def endOfBuffer () = view.point() = buffer.end

  @Fn("""Reads line number from minibuffer and goes to that line, counting from line 1 at
         beginning of buffer. Also centers the view on the requested line. If the mark is inactive,
         it will be set to the point prior to moving to the new line. """)
  def gotoLine () {
    window.mini.read("Goto line:", "", gotoLineHistory, Completer.none) onSuccess { lstr =>
      val line = try { lstr.toInt } catch {
        case e :Throwable => 1 // this is what emacs does, seems fine to me
      }
      if (!buffer.mark.isDefined) buffer.mark = view.point()
      view.point() = Loc(line-1, 0)
      recenter()
    }
  }

  @Fn("""Reads character offset from minibuffer and goes to that offset. Also centers the view on
         the requested line. If the mark is inactive, it will be set to the point prior to moving
         to the new line. """)
  def gotoOffset () {
    window.mini.read("Goto offset:", "", gotoLineHistory, Completer.none) onSuccess { offStr =>
      val offset = try { offStr.toInt } catch {
        case e :Throwable => 0 // this is what emacs does, seems fine to me
      }
      if (!buffer.mark.isDefined) buffer.mark = view.point()
      view.point() = buffer.loc(offset)
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
  def scrollUpPage () = view.scrollVert(1-view.height())

  @Fn("Scrolls the view down one page.")
  def scrollDownPage () = view.scrollVert(view.height()-1)

  @Fn("""Adjusts the scroll offset of the current window so that the line that contains the point
         is centered therein.""")
  def recenter () = view.scrollTop() = math.max(view.point().row - view.height()/2, 0)

  //
  // BUFFER FNS

  @Fn("""Reloads the current buffer from its source and restores the current point and scroll
         position. This is mainly useful when hacking on Scaled as it recreates the buffer from
         scratch reinitializing the major and minor modes. Note: this only works on clean buffers
         loaded from files. Modified or ephemeral buffers will refuse to be reloaded.""")
  def reloadBuffer () {
    if (buffer.dirty) window.popStatus("Cannot reload modified buffer.")
    else if (!buffer.store.exists) window.popStatus("Cannot reload ephemeral buffers.")
    else frame.revisitFile()
  }

  //
  // HELP FNS

  @Fn("Displays the styles at the point.")
  def showStyles () {
    val p = view.point()
    val info = Seq(buffer.stylesAt(p).toString)
    view.popup() = Popup.text(info, Popup.UpRight(p))
  }

  @Fn("Displays the tags at the point.")
  def showTags () {
    val p = view.point()
    val info = buffer.tagsAt(p) match {
      case Nil  => List("No tags.")
      case tags => tags.map(_.toString)
    }
    view.popup() = Popup.text(info, Popup.UpRight(p))
  }

  @Fn("Displays all tags on the current line.")
  def showLineTags () {
    val p = view.point()
    val line = buffer.line(p)
    val info = (line.lineTags ++ line.tags) match {
      case Nil  => List("No tags.")
      case tags => tags.map(_.toString)
    }
    view.popup() = Popup.text(info, Popup.UpRight(p))
  }

  //
  // META FNS

  @Fn("Reports that a key sequence is unknown.")
  def unknownCommand (trigger :String) {
    window.popStatus(s"$trigger is undefined.")
  }

  /** The history ring used for config var values. */
  protected def gotoLineHistory = Workspace.historyRing(wspace, "goto-line")
}
