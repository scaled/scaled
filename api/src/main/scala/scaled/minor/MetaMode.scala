//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._
import scaled.util.{BufferBuilder, SubProcess}
import java.nio.file.Paths

/** Defines fns that are not related to the current buffer, but rather for interation with the
  * editor environment or other global things. */
@Minor(name="meta", tags=Array("*"), desc="""Provides global meta fns.""")
class MetaMode (env :Env) extends MinorMode(env) {
  import Editor._

  override def keymap = super.keymap.
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

    // misc commands
    bind("M-x", "execute-extended-command");

  /** Queries the user for the name of a config var and invokes `fn` on the chosen var. */
  def withConfigVar (fn :Config.VarBind[_] => Unit) {
    val vars = disp.modes.flatMap(m => m.varBindings)
    val comp = Completer.from(vars)(_.v.name)
    editor.mini.read("Var:", "", varHistory(editor), comp) onSuccess fn
  }

  //
  // BUFFER FNS

  @Fn("""Reads a buffer name from the minibuffer and switches to it.""")
  def switchToBuffer () {
    val fstb = editor.buffers.head
    val defb = editor.buffers.drop(1).headOption getOrElse fstb
    val defp = s" (default ${defb.name})"
    val comp = Completer.buffer(editor, Some(defb), Set(fstb))
    editor.mini.read(s"Switch to buffer$defp:", "", bufferHistory(editor),
                     comp) onSuccess editor.visitBuffer
  }

  @Fn("""Reads a buffer name from the minibuffer and kills (closes) it.""")
  def killBuffer () {
    val current = editor.buffers.head
    val prompt = s"Kill buffer (default ${current.name}):"
    val comp = Completer.buffer(editor, Some(current))
    editor.mini.read(prompt, "", bufferHistory(editor), comp) onSuccess editor.killBuffer

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("""Reads a filename from the minibuffer and visits it in a buffer.""")
  def findFile () {
    editor.mini.read("Find file:", buffer.store.parent, fileHistory(editor),
                     Completer.file) onSuccess editor.visitFile
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
    editor.mini.read("Fn:", "", fnHistory(editor), Completer.from(disp.fns, true)) onSuccess { fn =>
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
      editor.mini.read(prompt, b.current, setVarHistory(editor), Completer.none) onSuccess { nv =>
        try b.update(nv) catch {
          case e :Exception => editor.popStatus(s"Unable to parse '$nv':", e.toString)
        }
      }
    }
  }

  @Fn("Describes the current major mode along with all of its key bindings.")
  def describeMode () {
    val bb = new BufferBuilder(this.view.width()-1)
    disp.modes foreach { m =>
      bb.addHeader(s"${m.name}-mode:")
      bb.addFilled(m.desc)
      bb.add(s"(tags: ${m.tags.mkString(" ")})")

      val keys = m.keymap.bindings
      if (!keys.isEmpty) {
        bb.addSubHeader("Key sequence    Binding")
        keys.sortBy(_.trigger) foreach {
          case Key.Binding(t, fn) => bb.add("%-15s %s".format(t, fn))
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
    val bb = new BufferBuilder(this.view.width()-1)
    def addState (state :StateV) {
      val kvs = state.keys.toSeq.flatMap(
        k => state.get(k).map(_.toString).map(v => (s"${k.getName}: " -> v)))
      if (!kvs.isEmpty) {
        bb.addSection("State")
        bb.addKeysValues(kvs)
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

  //
  // MISC FNS

  @Fn("Reads fn name then invokes it.")
  def executeExtendedCommand () {
    editor.mini.read("M-x", "", fnHistory(editor), Completer.from(disp.fns, true)) onSuccess { fn =>
      if (!disp.invoke(fn)) editor.popStatus(s"Unknown fn: $fn")
    }
  }

  @Fn("""Invokes a shell command and displays its output.
         The cwd will be the directory that contains the currently visited buffer.""")
  def shellCommand () {
    editor.mini.read("Command", "", shellCommandHistory, Completer.none) onSuccess { cmd =>
      def parseCmd (cmd :String) = cmd.split(" ") // TODO: handle quoted args
      val cfg = SubProcess.Config(parseCmd(cmd), cwd=Paths.get(buffer.store.parent))
      val view = editor.bufferConfig(s"*exec:${cfg.cmd(0)}*").mode("text").reuse().create()
      SubProcess(cfg, env.exec, view.buffer)
      editor.visitBuffer(view.buffer)
    }
  }

  @Fn("Toggles the activation of a minor mode.")
  def toggleMode () {
    val comp = Completer.from(disp.minorModes, true)
    editor.mini.read("Mode:", "", modeHistory(editor), comp) onSuccess disp.toggleMode
  }

  @Fn("Opens the configuration file for the specified mode in a buffer.")
  def editModeConfig () {
    val mcomp = Completer.from(disp.majorModes ++ disp.minorModes, true)
    editor.mini.read("Mode:", name, modeHistory(editor), mcomp) onSuccess editConfig
  }

  @Fn("Opens the configuration file for the Scaled editor in a buffer.")
  def editEditorConfig () :Unit = editConfig("editor")

  private def configScopeHistory = historyRing(editor, "config-scope")
  private def shellCommandHistory = historyRing(editor, "shell-command")

  private def editConfig (mode :String) {
    val scopes = editor.configScope(buffer).toList
    editor.mini.read("Scope:", scopes.head.name, configScopeHistory,
                     Completer.from(scopes)(_.name)) onSuccess { scope =>
      editor.visitConfig(scope, mode)
    }
  }
}
