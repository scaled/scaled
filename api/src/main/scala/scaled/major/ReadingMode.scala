//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import reactual.{Future, Promise}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.util.{BufferBuilder, Chars, Paragrapher}

/** Configuration for [[ReadingMode]]. */
object ReadingConfig extends Config.Defs {
  import EditorConfig._

  /** The history ring for goto-line. */
  val gotoLineHistory = fnKey(cfg => new Ring(cfg(historySize)))
}

/** A base class for major modes which allow text to be read but not edited. Motion and other
  * non-editing commands are defined here, but editing commands are deferred to [[EditingMode]]
  * which extends this one.
  */
abstract class ReadingMode (env :Env) extends MajorMode(env) {
  import Chars._
  import ReadingConfig._
  import EditorConfig._

  override def missedFn  :Option[String] = Some("unknown-command")

  override def configDefs = ReadingConfig :: super.configDefs
  override def keymap = super.keymap.
    // mark manipulation commands
    bind("C-SPACE", "set-mark-command"). // TODO: make this push-mark instead?
    bind("C-@",     "set-mark-command"). // needs to be C-S-2? meh.
    bind("C-x C-x", "exchange-point-and-mark").

    // the one non-destructive kill command
    bind("M-w", "kill-ring-save").

    // searching commands
    bind("C-s", "isearch-forward").
    bind("C-r", "isearch-backward").

    // motion commands
    bind("C-b",   "backward-char").
    bind("C-f",   "forward-char").
    bind("LEFT",  "backward-char").
    bind("RIGHT", "forward-char").

    bind("M-b",     "backward-word").
    bind("M-f",     "forward-word").
    bind("C-LEFT",  "backward-word").
    bind("C-RIGHT", "forward-word").

    bind("C-a",  "move-beginning-of-line").
    bind("C-e",  "move-end-of-line").
    bind("HOME", "move-beginning-of-line").
    bind("END",  "move-end-of-line").

    bind("C-p",  "previous-line").
    bind("C-n",  "next-line").
    bind("UP",   "previous-line").
    bind("DOWN", "next-line").

    bind("C-UP",   "previous-paragraph").
    bind("C-DOWN", "next-paragraph").

    bind("M-<",    "beginning-of-buffer").
    bind("M->",    "end-of-buffer").
    bind("C-HOME", "beginning-of-buffer").
    bind("C-END",  "end-of-buffer").
    bind("BEGIN",  "beginning-of-buffer").
    // TEMP: until we sort out meta'd shifted keys
    bind("M-S-,",  "beginning-of-buffer").
    bind("M-S-.",  "end-of-buffer").

    bind("M-g",    "goto-line").
    bind("M-S-g",  "goto-offset").

    // view commands (scrolling, etc.)
    bind("S-UP",   "scroll-up"). // TODO: extend-mark-backward-line
    bind("S-DOWN", "scroll-down"). // TODO: extend-mark-forward-line
    bind("M-v",    "scroll-up-page").
    bind("C-v",    "scroll-down-page").
    bind("PGUP",   "scroll-up-page").
    bind("PGDN",   "scroll-down-page").

    bind("C-l",    "recenter").

    // buffer commands
    bind("C-x b",   "switch-to-buffer").
    bind("C-x k",   "kill-buffer").
    bind("C-x C-f", "find-file").

    // editor commands
    bind("C-x C-c", "save-buffers-kill-editor").

    // help commands
    bind("C-h f", "describe-fn").
    bind("C-h v", "describe-var").
    bind("C-h m", "describe-mode").
    bind("C-h e", "describe-editor").
    bind("M-A-t", "show-tags").
    bind("M-C-t", "show-line-tags").

    // meta commands
    bind("M-x", "execute-extended-command");

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
    case None => editor.popStatus("The mark is not set now, so there is no region.")
    case Some(mp) => val p = view.point() ; if (mp < p) fn(mp, p) else fn(p, mp)
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
      case None => editor.popStatus("Unable to find a paragraph.")
      case Some(r) => fn(r.start, r.end)
    }
  }

  /** Queries the user for the name of a config var and invokes `fn` on the chosen var. */
  def withConfigVar (fn :Config.VarBind[_] => Unit) {
    val vars = disp.modes.flatMap(m => m.varBindings)
    val comp = Completer.from(vars)(_.v.name)
    editor.mini.read("Var:", "", config(varHistory), comp) onSuccess { vname =>
      vars.find(_.v.name == vname) match {
        case Some(v) => fn(v)
        case None    => editor.popStatus(s"No such var: $vname")
      }
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
    config(killRing) add buffer.region(start, end)
    editor.emitStatus("Region added to kill ring.")
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
    editor.statusMini("isearch", Promise[Boolean](), view, disp, "forward")
  }

  @Fn("Searches incrementally backward. See the command isearch-forward for more info.")
  def isearchBackward () {
    editor.statusMini("isearch", Promise[Boolean](), view, disp, "backward")
  }

  //
  // MARK MANIPULATION FNS

  @Fn("Sets the mark to the current point.")
  def setMarkCommand () {
    // TODO: push old mark onto local (buffer?) and global mark ring?
    buffer.mark = view.point()
    editor.emitStatus("Mark set.")
  }

  @Fn("Sets the mark to the current point and moves the point to the previous mark.")
  def exchangePointAndMark () {
    buffer.mark match {
      case Some(m) =>
        buffer.mark = view.point()
        view.point() = m
      case None =>
        editor.popStatus("No mark set in this buffer.")
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
    if (old == view.point()) editor.emitStatus("End of buffer.")
  }

  @Fn("Moves the point backward one character.")
  def backwardChar () {
    val old = view.point()
    view.point() = buffer.backward(old, 1)
    if (old == view.point()) editor.emitStatus("Beginning of buffer.")
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
    if (old == view.point()) editor.emitStatus("End of buffer.") // TODO: with beep?
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
    if (old == view.point()) editor.emitStatus("Beginning of buffer.") // TODO: with beep?
    desiredColumn = oldDesiredColumn
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
    view.point() = seek(view.point().row, false)
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
    editor.mini.read("Goto line:", "", config(gotoLineHistory), Completer.none) onSuccess { lstr =>
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
    val hist = config(gotoLineHistory)
    editor.mini.read("Goto offset:", "", hist, Completer.none) onSuccess { offStr =>
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

  @Fn("""Reads a buffer name from the minibuffer and switches to it.""")
  def switchToBuffer () {
    val fstb = editor.buffers.head
    val defb = editor.buffers.drop(1).headOption getOrElse fstb
    val defp = s" (default ${defb.name})"
    val comp = Completer.buffer(editor, Some(defb), Set(fstb))
    editor.mini.read(s"Switch to buffer$defp:", "", config(bufferHistory),
                     comp) onSuccess editor.visitBuffer
  }

  @Fn("""Reads a buffer name from the minibuffer and kills (closes) it.""")
  def killBuffer () {
    val current = editor.buffers.head
    val prompt = s"Kill buffer (default ${current.name}):"
    val comp = Completer.buffer(editor, Some(current))
    editor.mini.read(prompt, "", config(bufferHistory), comp) onSuccess editor.killBuffer

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("""Reads a filename from the minibuffer and visits it in a buffer.""")
  def findFile () {
    editor.mini.read("Find file:", buffer.store.parent, config(fileHistory),
                     Completer.file) onSuccess editor.visitFile
  }

  @Fn("""Reloads the current buffer from its source and restores the current point and scroll
         position. This is mainly useful when hacking on Scaled as it recreates the buffer from
         scratch reinitializing the major and minor modes. Note: this only works on clean buffers
         loaded from files. Modified or ephemeral buffers will refuse to be reloaded.""")
  def reloadBuffer () {
    if (buffer.dirty) editor.popStatus("Cannot reload modified buffer.")
    else if (!buffer.store.exists) editor.popStatus("Cannot reload ephemeral buffers.")
    else {
      val file = buffer.store ; val p = view.point()
      val top = view.scrollTop() ; val left = view.scrollLeft()
      editor.killBuffer(buffer)
      val nv = editor.visitFile(file)
      nv.scrollTop() = top
      nv.scrollLeft() = left
      nv.point() = p
    }
  }

  //
  // EDITOR FNS

  @Fn("""Offers to save any unsaved buffers, then kills this editor.""")
  def saveBuffersKillEditor () {
    val opts = Seq(
      "y"   -> "save the current buffer",
      "n"   -> "skip the current buffer (abandon changes)",
      "q"   -> "skip all remaining buffers",
      "!"   -> "save all remaining buffers",
      "."   -> "save *only* the current buffer, then exit",
      "C-g" -> "cancel this kill-editor command"
      // TODO: C-r to view this buffer?
      // TODO: d to diff this buffer against the file system version
    )
    def saveLoop (dirty :List[Buffer]) :Unit = dirty match {
      case Nil => editor.exit()
      case buf :: tail =>
        val prompt = s"${buf.store} is modified. Save?"
        editor.mini.readOpt(prompt, opts) onSuccess(_ match {
          case "y" => buf.save() ; saveLoop(tail)
          case "n" => saveLoop(tail)
          case "q" => saveLoop(Nil)
          case "!" => dirty map(_.save()) ; saveLoop(Nil)
          case "." => buf.save() ; saveLoop(Nil)
        })
    }
    saveLoop(editor.buffers.filter(_.needsSave).toList)
  }

  //
  // HELP FNS

  @Fn("Displays the documentation for a fn.")
  def describeFn () {
    editor.mini.read("Fn:", "", config(fnHistory), Completer.from(disp.fns, true)) onSuccess { fn =>
      disp.describeFn(fn) match {
        case Some(descrip) => editor.popStatus(s"Fn: $fn", descrip)
        case None => editor.popStatus(s"No such fn: $fn")
      }
    }
  }

  @Fn("Displays the documentation for a config var as well as its current value.")
  def describeVar () {
    withConfigVar(b => editor.popStatus(
      s"Mode: ${b.m.name}\nVar: ${b.v.name} (currently: ${b.current})", b.v.descrip))
  }

  @Fn("""Updates the in-memory value of a config var. The value is not persisted across sessions.
         Use edit-mode-config to make permanent changes.""")
  def setVar () {
    withConfigVar { b =>
      val prompt = s"Set ${b.v.name} to (current ${b.current}):"
      editor.mini.read(prompt, b.current, config(setVarHistory), Completer.none) onSuccess { nv =>
        try b.update(nv) catch {
          case e :Exception => editor.popStatus(s"Unable to parse '$nv':", e.toString)
        }
      }
    }
  }

  @Fn("Describes the current major mode along with all of its key bindings.")
  def describeMode () {
    val keysByMode = disp.triggers.groupBy(_._1)
    val bb = new BufferBuilder(fillColumn)
    disp.modes foreach { m =>
      bb.addHeader(s"${m.name}-mode:")
      bb.addFilled(m.desc)
      bb.add(s"(tags: ${m.tags.mkString(" ")})")

      keysByMode.get(m.name) map { keys =>
        bb.addSubHeader("Key sequence    Binding")
        keys.sorted foreach {
          case (m, t, fn) => bb.add("%-15s %s".format(t, fn))
        }
      }

      val vbs = m.varBindings
      if (!vbs.isEmpty) {
        bb.addSubHeader("Config vars")
        vbs.map(vb => (vb.v.name, vb.current, vb.v.descrip)).sorted foreach {
          case (n, c, d) => bb.addKeyValue(s"$n = ", c).addPreFilled("  ", d)
        }
      }
    }

    val major = disp.modes.last
    val view = editor.bufferConfig(s"*mode:${major.name}*").mode("help").reuse().create()
    editor.visitBuffer(bb.applyTo(view))
  }

  @Fn("Describes the current state of the editor. This is mainly for debugging and the curious.")
  def describeEditor () {
    val bb = new BufferBuilder(fillColumn)
    def addState (state :StateV) {
      val kvs = state.keys.toSeq.map(
        k => (s"${k.getName}: " -> state.get(k).map(_.toString).getOrElse("")))
      if (!kvs.isEmpty) {
        bb.addSection("State")
        bb.addKeysValues(kvs :_*)
      }
    }
    bb.addHeader("Editor")
    bb.addKeysValues("Buffers: " -> editor.buffers.size.toString)
    addState(editor.state)

    val ws = editor.workspace
    bb.addHeader("Workspace")
    bb.addKeysValues("Name: " -> ws.name,
                     "Root: " -> ws.root.toString)
    addState(ws.state)

    bb.addHeader("Buffers")
    editor.buffers.foreach { buf =>
      bb.addSubHeader(buf.name)
      bb.addKeysValues("Store: " -> buf.store.toString,
                       "Length: " -> buf.offset(buf.end).toString)
      addState(buf.state)
    }

    val view = editor.bufferConfig(s"*editor*").mode("help").reuse().create()
    editor.visitBuffer(bb.applyTo(view))
  }

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
      case Nil => List("No tags.")
      case tags => tags.map(_.toString)
    }
    view.popup() = Popup.text(info, Popup.UpRight(p))
  }

  @Fn("Displays all tags on the current line.")
  def showLineTags () {
    val p = view.point()
    view.popup() = Popup.text(buffer.line(p).tags.map(_.toString), Popup.UpRight(p))
  }

  //
  // META FNS

  @Fn("Reports that a key sequence is unknown.")
  def unknownCommand (trigger :String) {
    editor.popStatus(s"$trigger is undefined.")
  }

  @Fn("Reads fn name then invokes it.")
  def executeExtendedCommand () {
    editor.mini.read("M-x", "", config(fnHistory), Completer.from(disp.fns, true)) onSuccess { fn =>
      if (!disp.invoke(fn)) editor.popStatus(s"Unknown fn: $fn")
    }
  }

  @Fn("Toggles the activation of a minor mode.")
  def toggleMode () {
    val comp = Completer.from(disp.minorModes, true)
    editor.mini.read("Mode:", "", config(modeHistory), comp) onSuccess disp.toggleMode
  }

  @Fn("Opens the configuration file for the specified mode in a buffer.")
  def editModeConfig () {
    val comp = Completer.from(disp.majorModes ++ disp.minorModes, true)
    editor.mini.read("Mode:", name, config(modeHistory), comp) onSuccess editor.visitConfig
  }

  @Fn("Opens the configuration file for the Scaled editor in a buffer.")
  def editEditorConfig () {
    editor.visitConfig("editor")
  }
}
