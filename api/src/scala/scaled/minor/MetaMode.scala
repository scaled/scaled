//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import java.nio.file.Paths
import scaled._
import scaled.major.TextConfig
import scaled.util.{BufferBuilder, SubProcess}

/** Defines fns that are not related to the current buffer, but rather for interation with the
  * editor environment or other global things. */
@Minor(name="meta", tags=Array("*"), desc="""Provides global meta fns.""")
class MetaMode (env :Env) extends MinorMode(env) {
  import Workspace._

  override def keymap = super.keymap.
    // buffer commands
    bind("switch-to-buffer", "C-x b").
    bind("kill-buffer",      "C-x k").
    bind("find-file",        "C-x C-f").
    bind("create-file",      "C-x C-n").

    // editor commands
    bind("save-buffers-close-window", "C-x C-c").

    // visit-related commands
    bind("visit-next", "C-]").
    bind("visit-prev", "C-[").
    bind("visit-pop",  "M-,").

    // help commands
    bind("describe-fn",     "C-h f").
    bind("describe-var",    "C-h v").
    bind("describe-mode",   "C-h m").
    bind("describe-editor", "C-h e").

    // misc commands
    bind("execute-extended-command", "M-x");

  /** Queries the user for the name of a config var and invokes `fn` on the chosen var. */
  def withConfigVar (fn :JConsumer[Config.VarBind[_]]) :Unit = {
    val vars = disp.modes.flatMap(m => m.varBindings)
    val comp = Completer.from(vars)(_.v.name)
    window.mini.read("Var:", "", varHistory(wspace), comp) onSuccess fn
  }

  //
  // BUFFER FNS

  @Fn("""Reads a buffer name from the minibuffer and switches to it.""")
  def switchToBuffer () :Unit = {
    val curb = buffer
    // if we're the only window in the workspace, use the workspace buffer list (so we get
    // potentially never visited buffers like *messages*), otherwise use this window's visited
    // buffers list
    val buffers = if (wspace.windows.size == 1) wspace.buffers else window.buffers
    // if the current frame has a previous buffer, and it is still open, use that as our default
    // buffer, otherwise fallback to the top buffer in the window buffers list (skipping our
    // current buffer), and if all else fails, just use our current buffer
    val defb = window.focus.prevStore.flatMap(s => buffers.find(_.store == s)) orElse
      buffers.filter(_ != curb).headOption getOrElse curb
    val comp = Completer.buffer(buffers, defb, Set(curb))
    window.mini.read(s"Switch to buffer (default ${defb.name}):", "", bufferHistory, comp).
      onSuccess(buf => frame.visit(buf))
  }

  @Fn("Reads a buffer name from the minibuffer and kills (closes) it.")
  def killBuffer () :Unit = {
    val current = buffer
    val prompt = s"Kill buffer (default ${current.name}):"
    val comp = Completer.buffer(window.buffers, current)
    window.mini.read(prompt, "", bufferHistory, comp).onSuccess(_.kill())

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("Reads a filename from the minibuffer and visits it in a buffer.")
  def findFile () :Unit = {
    window.mini.read("Find file:", buffer.store.parent, fileHistory(wspace),
                     Completer.file(editor.exec)) onSuccess frame.visitFile
  }

  @Fn("Reads a filename from the minibuffer and visits it in a buffer. The file need not exist.")
  def createFile () :Unit = {
    window.mini.read("File path:", buffer.store.parent, createFileHistory,
                     Completer.none) onSuccess { path => frame.visitFile(Store(path)) }
  }
  private val createFileHistory = new Ring(16)

  //
  // EDITOR FNS

  @Fn("""Offers to save any unsaved buffers, then closes this window.
         If this is the last open window, the editor will edit.""")
  def saveBuffersCloseWindow () :Unit = window.saveBuffersAndClose()

  //
  // VISIT FNS

  @Fn("Navigates to the next entry in the current visit list, if any.")
  def visitNext () :Unit = {
    window.visits().next(window)
  }

  @Fn("Navigates to the previous entry in the current visit list, if any.")
  def visitPrev () :Unit = {
    window.visits().prev(window)
  }

  @Fn("Pops the top location from the visit stack and returns to it.")
  def visitPop () :Unit = {
    window.visitStack.pop(window)
  }

  //
  // HELP FNS

  @Fn("Displays the documentation for a fn.")
  def describeFn () :Unit = {
    window.mini.read("Fn:", "", fnHistory(wspace), Completer.from(disp.fns)) onSuccess { fn =>
      disp.describeFn(fn) match {
        case Some(descrip) => window.popStatus(s"Fn: $fn", descrip)
        case None => window.popStatus(s"No such fn: $fn")
      }
    }
  }

  @Fn("Displays the documentation for a config var as well as its current value.")
  def describeVar () :Unit = {
    withConfigVar(b => window.popStatus(
      s"Mode: ${b.m.name}\nVar: ${b.v.name} (currently: ${b.current})", b.v.descrip))
  }

  @Fn("""Updates the in-memory value of a config var. The value is not persisted across sessions.
         Use edit-mode-config to make permanent changes.""")
  def setVar () :Unit = {
    withConfigVar { b =>
      val prompt = s"Set ${b.v.name} to (current ${b.current}):"
      window.mini.read(prompt, b.current, setVarHistory(wspace), Completer.none) onSuccess { nv =>
        try b.update(nv) catch {
          case e :Exception => window.popStatus(s"Unable to parse '$nv':", e.toString)
        }
      }
    }
  }

  @Fn("Shows the *messages* buffer.")
  def showMessages () = wspace.buffers.find(_.name == "*messages*") match {
    case Some(buffer) => window.focus.visit(buffer)
    case None => window.popStatus("Unable to find *messages* buffer?")
  }

  @Fn("Describes the current major mode along with all of its key bindings.")
  def describeMode () :Unit = {
    val bb = new BufferBuilder(this.view.width()-1)
    def describe (m :Mode) :Unit = {
      bb.addHeader(s"${m.name}-mode:")
      // split the description into double newline separated paragraphs, and fill those
      // individually
      m.desc.split("\n\n") foreach { p =>
        var inPre = false
        p.split("```") foreach { sec =>
          val tsec = sec.trim
          if (inPre) tsec.split("\n") foreach { l => bb.add(l, TextConfig.listStyle) }
          else if (tsec.length > 0) bb.addFilled(tsec)
          inPre = !inPre
        }
        bb.addBlank()
      }
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

    // describe the major mode first, then the minor modes
    val major = disp.modes.last
    describe(major)
    disp.modes filter(_ != major) foreach describe

    val hbuf = wspace.createBuffer(Store.scratch(s"*mode:${major.name}*", buffer.store),
                                   reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  @Fn("Describes the current state of the editor. This is mainly for debugging and the curious.")
  def describeEditor () :Unit = {
    val bb = new BufferBuilder(this.view.width()-1)
    bb.addHeader("Editor")
    editor.state.describeSelf(bb)

    env.msvc.service[PackageService].describePackages(bb)

    def defang (text :String) = text.replace("\n", "\\n")
    bb.addHeader("System Properties")
    val sysprops = System.getProperties
    val spKeys = Ordered.view(sysprops.stringPropertyNames).sorted
    bb.addKeysValues(spKeys.map(name => (s"$name ", defang(sysprops.getProperty(name)))))

    bb.addHeader("Environment")
    val sysenv = System.getenv
    val seKeys = Ordered.view(sysenv.keySet).sorted
    bb.addKeysValues(seKeys.map(name => (s"$name ", defang(sysenv.get(name)))))

    bb.addHeader("Runtime")
    val rt = Runtime.getRuntime
    bb.addKeysValues(
      "Max memory: " -> rt.maxMemory,
      "Total memory: " -> rt.totalMemory,
      "Free memory: " -> rt.freeMemory,
      "Processors: " -> rt.availableProcessors
    )

    val hbuf = wspace.createBuffer(Store.scratch(s"*editor*", buffer.store),
                                   reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  //
  // MISC FNS

  @Fn("Reads fn name then invokes it.")
  def executeExtendedCommand () :Unit = {
    window.mini.read("M-x", "", fnHistory(wspace), Completer.from(disp.fns)) onSuccess { fn =>
      if (!disp.invoke(fn)) window.popStatus(s"Unknown fn: $fn")
    }
  }

  @Fn("""Invokes a shell command and displays its output.
         The cwd will be the directory that contains the currently visited buffer.""")
  def shellCommand () :Unit = {
    window.mini.read("Command", "", shellCommandHistory, Completer.none) onSuccess { cmd =>
      def parseCmd (cmd :String) = cmd.split(" ") // TODO: handle quoted args
      val cfg = SubProcess.Config(parseCmd(cmd), cwd=Paths.get(buffer.store.parent))
      val ebuf = wspace.createBuffer(Store.scratch(s"*exec:${cfg.cmd(0)}*", buffer.store),
                                     reuse=true, state=State.inits(Mode.Hint("text")))
      SubProcess(cfg, window.exec, ebuf)
      frame.visit(ebuf)
    }
  }

  @Fn("Reports the geometry of the current window.")
  def windowGeom () :Unit = {
    window.emitStatus(window.geometry.toString)
  }

  @Fn("Reports the geometry of the current frame.")
  def frameGeom () :Unit = {
    window.emitStatus(window.focus.geometry.toString)
  }

  @Fn("Toggles the activation of a minor mode.")
  def toggleMode () :Unit = {
    val comp = Completer.from(disp.minorModes)
    window.mini.read("Mode:", "", modeHistory(wspace), comp) onSuccess disp.toggleMode
  }

  @Fn("Opens the configuration file for the Scaled editor in a buffer.")
  def editEditorConfig () :Unit = editConfig(wspace.config)

  @Fn("Opens the configuration file for the specified mode in a buffer.")
  def editModeConfig () :Unit = {
    val comp = Completer.from(disp.modes)(_.name)
    val mname = disp.modes.last.name // default to major mode name
    window.mini.read("Mode:", mname, modeHistory(wspace), comp) map(_.config) onSuccess(editConfig)
  }

  @Fn("Opens the configuration file for the specified service in a buffer.")
  def editServiceConfig () :Unit = {
    val comp = Completer.from(env.msvc.service[ConfigService].serviceConfigs)(_._1)
    window.mini.read("Service:", "", serviceHistory, comp) map(_._2) onSuccess(editConfig)
  }

  private def bufferHistory = window.historyRing("buffer")
  private def configScopeHistory = wspace.historyRing("config-scope")
  private def shellCommandHistory = wspace.historyRing("shell-command")
  private def serviceHistory = wspace.historyRing("service")

  private def editConfig (config :Config) :Unit = {
    val scopes = config.scope.toList ; val comp = Completer.from(scopes)(_.name)
    window.mini.read("Scope:", scopes.head.name, configScopeHistory, comp) onSuccess { scope =>
      val target = config.atScope(scope)
      val cbuf = wspace.openBuffer(Store(target.file))
      val content = target.toProperties
      if (content != cbuf.region(cbuf.start, cbuf.end).map(_.asString)) {
        cbuf.replace(cbuf.start, cbuf.end, content.map(Line.apply))
      }
      frame.visit(cbuf)
    }
  }
}
