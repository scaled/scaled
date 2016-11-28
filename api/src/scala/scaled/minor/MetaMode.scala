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
  def withConfigVar (fn :JConsumer[Config.VarBind[_]]) {
    val vars = disp.modes.flatMap(m => m.varBindings)
    val comp = Completer.from(vars)(_.v.name)
    window.mini.read("Var:", "", varHistory(wspace), comp) onSuccess fn
  }

  //
  // BUFFER FNS

  @Fn("""Reads a buffer name from the minibuffer and switches to it.""")
  def switchToBuffer () {
    val curb = buffer
    // if the current frame has a previous buffer, and it is still open, use that as our default
    // buffer, otherwise fallback to the top buffer in the window buffers list (skipping our
    // current buffer), and if all else fails, just use our current buffer
    val defb = window.focus.prevStore.flatMap(s => wspace.buffers.find(_.store == s)) orElse
      wspace.buffers.filter(_ != curb).headOption getOrElse curb
    val comp = Completer.buffer(wspace, defb, Set(curb))
    window.mini.read(s"Switch to buffer (default ${defb.name}):", "", bufferHistory(wspace),
                     comp) onSuccess frame.visit
  }

  @Fn("Reads a buffer name from the minibuffer and kills (closes) it.")
  def killBuffer () {
    val current = buffer
    val prompt = s"Kill buffer (default ${current.name}):"
    val comp = Completer.buffer(wspace, current)
    window.mini.read(prompt, "", bufferHistory(wspace), comp) onSuccess(_.kill())

    // TODO: document our process when we have one:

    // The functions in `kill-buffer-query-functions' are called with the buffer to be killed as
    // the current buffer. If any of them returns nil, the buffer is not killed. The hook
    // `kill-buffer-hook' is run before the buffer is actually killed. The buffer being killed will
    // be current while the hook is running. Functions called by any of these hooks are supposed to
    // not change the current buffer.
  }

  @Fn("Reads a filename from the minibuffer and visits it in a buffer.")
  def findFile () {
    window.mini.read("Find file:", buffer.store.parent, fileHistory(wspace),
                     Completer.file) onSuccess frame.visitFile
  }

  @Fn("Reads a filename from the minibuffer and visits it in a buffer. The file need not exist.")
  def createFile () {
    window.mini.read("File path:", buffer.store.parent, createFileHistory,
                     Completer.none) onSuccess { path => frame.visitFile(Store(path)) }
  }
  private val createFileHistory = new Ring(16)

  //
  // EDITOR FNS

  @Fn("""Offers to save any unsaved buffers, then closes this window.
         If this is the last open window, the editor will edit.""")
  def saveBuffersCloseWindow () {
    val opts = Seq(
      "y"   -> "save the current buffer",
      "n"   -> "skip the current buffer (abandon changes)",
      "q"   -> "skip all remaining buffers",
      "!"   -> "save all remaining buffers",
      "."   -> "save *only* the current buffer, then close",
      "C-g" -> "cancel this close-window command"
      // TODO: C-r to view this buffer?
      // TODO: d to diff this buffer against the file system version
    )
    def saveLoop (dirty :List[Buffer]) :Unit = dirty match {
      case Nil => window.close()
      case buf :: tail =>
        val prompt = s"${buf.store} is modified. Save?"
        window.mini.readOpt(prompt, opts) onSuccess(_ match {
          case "y" => buf.save() ; saveLoop(tail)
          case "n" => saveLoop(tail)
          case "q" => saveLoop(Nil)
          case "!" => dirty map(_.save()) ; saveLoop(Nil)
          case "." => buf.save() ; saveLoop(Nil)
        })
    }
    saveLoop(wspace.buffers.filter(_.needsSave).toList)
  }

  //
  // VISIT FNS

  @Fn("Navigates to the next entry in the current visit list, if any.")
  def visitNext () {
    window.visits.getOption match {
      case None     => window.popStatus("No visit list is currently configured.")
      case Some(vs) => vs.next(window)
    }
  }

  @Fn("Navigates to the previous entry in the current visit list, if any.")
  def visitPrev () {
    window.visits.getOption match {
      case None     => window.popStatus("No visit list is currently configured.")
      case Some(vs) => vs.prev(window)
    }
  }

  @Fn("Pops the top location from the visit stack and returns to it.")
  def visitPop () {
    window.visitStack.pop(window)
  }

  //
  // HELP FNS

  @Fn("Displays the documentation for a fn.")
  def describeFn () {
    window.mini.read("Fn:", "", fnHistory(wspace), Completer.from(disp.fns)) onSuccess { fn =>
      disp.describeFn(fn) match {
        case Some(descrip) => window.popStatus(s"Fn: $fn", descrip)
        case None => window.popStatus(s"No such fn: $fn")
      }
    }
  }

  @Fn("Displays the documentation for a config var as well as its current value.")
  def describeVar () {
    withConfigVar(b => window.popStatus(
      s"Mode: ${b.m.name}\nVar: ${b.v.name} (currently: ${b.current})", b.v.descrip))
  }

  @Fn("""Updates the in-memory value of a config var. The value is not persisted across sessions.
         Use edit-mode-config to make permanent changes.""")
  def setVar () {
    withConfigVar { b =>
      val prompt = s"Set ${b.v.name} to (current ${b.current}):"
      window.mini.read(prompt, b.current, setVarHistory(wspace), Completer.none) onSuccess { nv =>
        try b.update(nv) catch {
          case e :Exception => window.popStatus(s"Unable to parse '$nv':", e.toString)
        }
      }
    }
  }

  @Fn("Describes the current major mode along with all of its key bindings.")
  def describeMode () {
    val bb = new BufferBuilder(this.view.width()-1)
    def describe (m :Mode) {
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
    addState(editor.state)

    bb.addHeader("Workspace")
    bb.addKeysValues("Name: " -> wspace.name,
                     "Root: " -> wspace.root.toString,
                     "Buffers: " -> wspace.buffers.size.toString)
    addState(wspace.state)

    bb.addHeader("Buffers")
    wspace.buffers.foreach { buf =>
      bb.addSubHeader(buf.name)
      bb.addKeysValues("Store: " -> buf.store.toString,
                       "Length: " -> buf.offset(buf.end).toString)
      addState(buf.state)
    }

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
  def executeExtendedCommand () {
    window.mini.read("M-x", "", fnHistory(wspace), Completer.from(disp.fns)) onSuccess { fn =>
      if (!disp.invoke(fn)) window.popStatus(s"Unknown fn: $fn")
    }
  }

  @Fn("""Invokes a shell command and displays its output.
         The cwd will be the directory that contains the currently visited buffer.""")
  def shellCommand () {
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
  def windowGeom () {
    window.emitStatus(window.geometry.toString)
  }

  @Fn("Reports the geometry of the current frame.")
  def frameGeom () {
    window.emitStatus(window.focus.geometry.toString)
  }

  @Fn("Toggles the activation of a minor mode.")
  def toggleMode () {
    val comp = Completer.from(disp.minorModes)
    window.mini.read("Mode:", "", modeHistory(wspace), comp) onSuccess disp.toggleMode
  }

  @Fn("Opens the configuration file for the Scaled editor in a buffer.")
  def editEditorConfig () :Unit = editConfig(wspace.config)

  @Fn("Opens the configuration file for the specified mode in a buffer.")
  def editModeConfig () {
    val comp = Completer.from(disp.modes)(_.name)
    val mname = disp.modes.last.name // default to major mode name
    window.mini.read("Mode:", mname, modeHistory(wspace), comp) map(_.config) onSuccess(editConfig)
  }

  @Fn("Opens the configuration file for the specified service in a buffer.")
  def editServiceConfig () {
    val comp = Completer.from(env.msvc.service[ConfigService].serviceConfigs)(_._1)
    window.mini.read("Service:", "", serviceHistory, comp) map(_._2) onSuccess(editConfig)
  }

  private def configScopeHistory = historyRing(wspace, "config-scope")
  private def shellCommandHistory = historyRing(wspace, "shell-command")
  private def serviceHistory = historyRing(wspace, "service")

  private def editConfig (config :Config) {
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
