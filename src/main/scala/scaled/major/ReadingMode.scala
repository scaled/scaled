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

  override def configDefs = EditingConfig :: super.configDefs
  override def keymap = Seq(

    // mark manipulation commands
    "C-SPACE" -> "set-mark-command", // TODO: make this push-mark instead?
    "C-@"     -> "set-mark-command", // needs to be C-S-2? meh.
    "C-x C-x" -> "exchange-point-and-mark",

    // the one non-destructive kill command
    "C-M-w"   -> "append-next-kill",

    // searching and replacing commands
    "C-s"   -> "isearch-forward",
    "C-r"   -> "isearch-backward",

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
    "C-x C-f" -> "find-file",

    // editor commands
    "C-x C-c" -> "save-buffers-kill-editor",

    // help commands
    "C-h f" -> "describe-fn",
    "C-h v" -> "describe-var",
    "C-h m" -> "describe-mode",

    // meta commands
    "M-x" -> "execute-extended-command"
  )

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
    else {
      val nonWordC = buffer.scanBackward(isNotWord, firstWordC)
      // we may have hit the beginning of the buffer looking for a non-word char; if so, cope
      if (isWord(buffer.charAt(nonWordC))) nonWordC else buffer.forward(nonWordC, 1)
    }
  }

  /** Invokes `fn` with the start and end of the current region. If the mark is not set, a status
    * message is emitted that indicates that the current region is not set and `fn` is not
    * invoked. */
  def withRegion (fn :(Loc, Loc) => Unit) :Unit = buffer.mark match {
    case None => editor.popStatus("The mark is not set now, so there is no region.")
    case Some(mp) => val p = view.point() ; if (mp < p) fn(mp, p) else fn(p, mp)
  }

  /** Queries the user for the name of a config var and invokes `fn` on the chosen var. */
  def withConfigVar (fn :Config.VarBind[_] => Unit) {
    val vars = disp.modes.flatMap(m => m.varBindings)
    val comp = Completer.from(vars, true)(_.v.name)
    editor.miniRead("Var:", "", config(varHistory), comp) onSuccess { vname =>
      vars.find(_.v.name == vname) match {
        case Some(v) => fn(v)
        case None    => editor.popStatus(s"No such var: $vname")
      }
    }
  }

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
    editor.mini("isearch", Promise[Boolean](), view, disp, "forward")
  }

  @Fn("Searches incrementally backward. See the command isearch-forward for more info.")
  def isearchBackward () {
    editor.mini("isearch", Promise[Boolean](), view, disp, "backward")
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

  @Fn("Moves the point down one line.")
  def nextLine () {
    val old = view.point()
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point() = old.nextL
    if (old == view.point()) editor.emitStatus("End of buffer.") // TODO: with beep?
  }

  @Fn("Moves the point up one line.")
  def previousLine () {
    val old = view.point()
    // TODO: emacs preserves the column we "want" to be in, we should do that too; maybe that
    // means not bounding the column, but instead coping with a current column that is beyond
    // the end of the current line (will that be a pandora's box?)
    view.point() = old.prevL
    if (old == view.point()) editor.emitStatus("Beginning of buffer.") // TODO: with beep?
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
    editor.miniRead("Goto line:", "", config(gotoLineHistory), Completer.none) onSuccess { lineStr =>
      val line = try { lineStr.toInt } catch {
        case e :Throwable => 1 // this is what emacs does, seems fine to me
      }
      if (!buffer.mark.isDefined) buffer.mark = view.point()
      view.point() = Loc(line-1, 0)
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
    val fstb = editor.buffers.head.name
    val defb = editor.buffers.drop(1).headOption.map(_.name) getOrElse fstb
    val defp = if (defb == "") "" else s" (default $defb)"
    val comp = Completer.buffer(editor, Set(fstb))
    editor.miniRead(s"Switch to buffer$defp:", "", config(bufferHistory), comp) onSuccess { read =>
      editor.openBuffer(if (read == "") defb else read)
    }
  }

  @Fn("""Reads a buffer name from the minibuffer and kills (closes) it.""")
  def killBuffer () {
    val current = editor.buffers.head.name
    val prompt = s"Kill buffer (default $current):"
    editor.miniRead(prompt, "", config(bufferHistory), Completer.buffer(editor)) onSuccess { read =>
      val buffer = if (read == "") current else read
      if (!editor.killBuffer(buffer)) editor.popStatus(s"No buffer named: $buffer")
    }

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("""Reads a filename from the minibuffer and visits it in a buffer.""")
  def findFile () {
    val bufwd = buffer.dir.getAbsolutePath + File.separator
    editor.miniRead("Find file:", bufwd, config(fileHistory), Completer.file) onSuccess { file =>
      if (file.isDirectory) editor.popStatus(
        "Scaled does not support editing directories. Use Emacs.")
      else editor.visitFile(file)
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
      case Nil => editor.exit(0)
      case buf :: tail =>
        val prompt = s"${buf.file.getAbsolutePath} is modified. Save?"
        editor.mini("readopt", Promise[String](), prompt, opts) onSuccess(_ match {
          case "y" => buf.save() ; saveLoop(tail)
          case "n" => saveLoop(tail)
          case "q" => saveLoop(Nil)
          case "!" => dirty map(_.save()) ; saveLoop(Nil)
          case "." => buf.save() ; saveLoop(Nil)
        })
    }
    saveLoop(editor.buffers.filter(_.dirty).toList)
  }

  //
  // HELP FNS

  @Fn("Displays the documentation for a fn.")
  def describeFn () {
    editor.miniRead("Fn:", "", config(fnHistory), Completer.from(disp.fns, true)) onSuccess { fn =>
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
      editor.miniRead(prompt, b.current, config(setVarHistory), Completer.none) onSuccess { newval =>
        try b.update(newval) catch {
          case e :Exception => editor.popStatus(s"Unable to parse '$newval':", e.toString)
        }
      }
    }
  }

  @Fn("Describes the current major mode along with all of its key bindings.")
  def describeMode () {
    val major = disp.modes.last
    val buf = editor.createBuffer(s"*${major.name}-mode*", "help", true).buffer
    val keysByMode = disp.triggers.groupBy(_._1)
    val text = new ArrayBuffer[String]()

    // TODO: wrap at the current width
    def format (text :String) = text.replaceAll("\n", "").replaceAll(" +", " ").trim

    disp.modes foreach { m =>
      val title = s"${m.name}-mode:"
      text += title
      text += "=" * title.length
      text += format(m.desc)
      text += s"(tags: ${m.tags.mkString(" ")})"

      keysByMode.get(m.name) map { keys =>
        text += ""
        text += "Key sequence    Binding"
        text += "------------    -------"
        keys.sorted foreach {
          case (m, t, fn) => text += "%-15s %s".format(t, fn)
        }
      }

      val vbs = m.varBindings
      if (!vbs.isEmpty) {
        text += ""
        text += "Config vars"
        text += "-----------"
        vbs.map(vb => (vb.v.name, vb.current, vb.v.descrip)).sorted foreach {
          case (n, c, d) => text += "%5s = %s".format(n, c) ; text += s"  $d"
        }
      }

      text += ""
    }

    buf.replace(buf.start, buf.end, text.map(new Line(_)))
    // TODO: put the buffer in a special "describe mode" mode
  }

  //
  // META FNS

  @Fn("Reports that a key sequence is unknown.")
  def unknownCommand (trigger :String) {
    editor.popStatus(s"$trigger is undefined.")
  }

  @Fn("Reads fn name then invokes it.")
  def executeExtendedCommand () {
    editor.miniRead("M-x", "", config(fnHistory), Completer.from(disp.fns, true)) onSuccess { fn =>
      if (!disp.invoke(fn)) editor.popStatus(s"Unknown fn: $fn")
    }
  }

  @Fn("Toggles the activation of a minor mode.")
  def toggleMode () {
    val comp = Completer.from(disp.minorModes, true)
    editor.miniRead("Mode:", "", config(modeHistory), comp) onSuccess disp.toggleMode
  }

  @Fn("Opens the configuration file for the specified mode in a buffer.")
  def editModeConfig () {
    val comp = Completer.from(disp.majorModes ++ disp.minorModes, true)
    editor.miniRead("Mode:", name, config(modeHistory), comp) onSuccess editor.visitConfig
  }

  @Fn("Opens the configuration file for the Scaled editor in a buffer.")
  def editEditorConfig () {
    editor.visitConfig("editor")
  }
}
