//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.{Files, Paths}
import javafx.application.{Application, Platform}
import javafx.beans.binding.Bindings
import javafx.geometry.{HPos, VPos}
import javafx.scene.control.Label
import javafx.scene.layout.{BorderPane, Region, VBox}
import javafx.stage.Stage
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.major.TextMode
import scaled.util.Errors

/** The editor pane groups together the various UI components that are needed to edit a single
  * buffer. This includes the code area, status line and minibuffer area. It also manages the
  * [[BufferView]] state and wires everything together.
  *
  * Multiple instances of an editor pane may be instantiated at the same time (and placed into tabs
  * or simply shown one at a time, depending on the user's configuration), but each editor pane is
  * largely an island unto itself.
  */
class EditorPane (val stage :Stage, ws :WorkspaceImpl, size :(Int, Int))
    extends Region with Editor {

  /** Used to resolve modes in this editor. */
  val resolver = new AppModeResolver(ws, this)

  /** Called when this editor pane is going away. Cleans up. */
  def dispose () {
    _buffers foreach { _.dispose() }
    _buffers.clear()
    _logcon.close()
    _wscon.close()
  }
  private val _wscon = ws.reference(this)

  //
  // Editor interface methods

  override def exit () = ws.close(this)
  override def showURL (url :String) = {
    // getHostSevices.showDocument is very crashy on Mac OS X right now, so avoid it
    if (System.getProperty("os.name") != "Mac OS X") ws.app.getHostServices.showDocument(url)
    else Runtime.getRuntime.exec(Array("open", url))
  }

  override def popStatus (msg :String, subtext :String) {
    _statusPopup.showStatus(msg, subtext)
    recordMessage(msg)
    if (subtext.length > 0) recordMessage(subtext)
  }
  override def emitStatus (msg :String, ephemeral :Boolean) {
    _statusLine.setText(msg)
    if (!ephemeral) recordMessage(msg)
  }
  override def emitError (err :Throwable) :Unit = err match {
    // if this is a wrapped reaction exception, unwrap
    case re :ReactionException => re.getSuppressed foreach(emitError)
    case _ =>
      // TODO: color the status label red or something
      popStatus(err.getMessage match {
        case null => err.toString
        case msg  => msg
      })
      if (!Errors.isFeedback(err)) {
        val trace = Errors.stackTraceToString(err)
        ws.app.debugLog(trace)
        recordMessage(trace)
      }
    }

  override def clearStatus () = {
    _statusPopup.clear()
    _statusLine.setText("")
    _active.view.clearEphemeralPopup()
  }

  override def mini = _mini
  override def statusMini = _statusMini
  override val state = new State()
  override def workspace = ws
  override def buffers = _buffers.map(_.buffer)

  override def visitFile (store :Store) = {
    _focus() = _buffers.find(_.buffer.store == store) getOrElse {
      if (!store.exists) emitStatus("(New file)")
      newBuffer(BufferImpl(store), bufferConfig(""))
    }
    _focus().view
  }
  override def visitBuffer (buffer :Buffer) = {
    _focus() = requireBuffer(buffer)
    _focus().view
  }
  override def visitConfig (mode :String) = {
    val view = visitFile(Store(ws.cfgMgr.configFile(mode)))
    ws.cfgMgr.configText(mode) match {
      case Some(lines) =>
        if (lines != view.buffer.region(view.buffer.start, view.buffer.end).map(_.asString)) {
          view.buffer.replace(view.buffer.start, view.buffer.end, lines.map(Line.apply))
        }
      case None => // TODO
    }
    view
  }

  override def createBuffer (config :BufferConfig) = {
    def create (name :String) = newBuffer(createEmptyBuffer(name, config._state), config)
    val ob = _buffers.find(_.name == config.name) match {
      case None     => create(config.name)
      case Some(ob) => if (config._reuse) ob else create(freshName(config.name))
    }
    ob.view
  }
  override def killBuffer (buffer :Buffer) = killBuffer(requireBuffer(buffer))

  // used internally to open files passed on the command line or via remote cmd
  def visitPath (path :String) {
    val p = Paths.get(path)
    visitFile(Store(if (Files.exists(p) || p.isAbsolute) p else cwd.resolve(path)))
    stageToFront()
  }
  def stageToFront () {
    stage.toFront() // move our window to front if it's not there already
    stage.requestFocus() // and request window manager focus
  }

  //
  // implementation details

  getStyleClass.add("editor")

  private case class OpenBuffer (pane :BorderPane,     area :BufferArea,
                                 view :BufferViewImpl, disp :DispatcherImpl) {
    def buffer = view.buffer
    def name = buffer.name
    def dispose () :Unit = disp.dispose()
    override def toString = name
  }
  private val _buffers = SeqBuffer[OpenBuffer]()

  private var _active :OpenBuffer = _
  private def setBuffer (buf :OpenBuffer) {
    if (_active == buf) _active.pane.toFront()
    else {
      if (_active != null) {
        getChildren.remove(_active.pane)
        _active.area.hibernate()
      }
      stage.setTitle(s"Scaled - ${workspace.name} - ${buf.name}")
      _active = buf

      val start = System.nanoTime() // TEMP: perf debugging
      getChildren.add(_active.pane)
      val elapsed = (System.nanoTime() - start)/1000
      if (elapsed > 500) println(s"EditorPane add BufferArea $elapsed us")
    }
  }

  private val _statusPopup = new StatusPopup()
  _statusPopup.maxWidthProperty.bind(Bindings.subtract(widthProperty, 20))
  getChildren.add(_statusPopup)

  private val _statusLine = new Label(" ")
  _statusLine.setWrapText(true)
  _statusLine.getStyleClass.add("status")
  getChildren.add(_statusLine)

  private val _mini = new MiniOverlay(this) {
    override def onClear () = _focus().area.requestFocus() // restore buffer focus on clear
  }
  _mini.maxWidthProperty.bind(Bindings.subtract(widthProperty, 20))
  getChildren.add(_mini)

  private val _statusMini = new MiniStatus(this) {
    override def onShow () {
      super.onShow()
      _statusLine.setVisible(false)
    }
    override def onClear () {
      _statusLine.setVisible(true)
      _focus().area.requestFocus() // restore buffer focus on clear
    }
  }
  getChildren.add(_statusMini)

  // we manage focus specially, via this reactive value
  private val _focus = Value[OpenBuffer](null)
  _focus onValue onFocusChange

  // create a *messages* and *scratch* buffer to start
  newMessages()
  _focus() = newScratch()

  // record all log messages to the *messages* buffer
  private val _logcon = ws.app.log.onValue(recordMessage)

  // we manage layout manually for a variety of nefarious reasons
  override protected def computeMinWidth (height :Double) = _active.pane.minWidth(height)
  override protected def computeMinHeight (width :Double) = _active.pane.minHeight(width)
  override protected def computePrefWidth (height :Double) = _active.pane.prefWidth(height)
  override protected def computePrefHeight (width :Double) = _active.pane.prefHeight(width)
  override protected def computeMaxWidth (height :Double) = Double.MaxValue
  override protected def computeMaxHeight (width :Double) = Double.MaxValue

  override def layoutChildren () {
    val bounds = getLayoutBounds
    val vw = bounds.getWidth ; val vh = bounds.getHeight
    val statusHeight = _statusLine.prefHeight(vw) ; val contentHeight = vh-statusHeight
    _active.pane.resize(vw, contentHeight)
    // the status line and status minibuffer occupy the same space; only one is visible at a time
    _statusLine.resizeRelocate(0, contentHeight, vw, statusHeight)
    _statusMini.resizeRelocate(0, contentHeight, vw, statusHeight)

    // the status overlay is centered in the top 1/4th of the screen
    if (_statusPopup.isVisible) layoutInArea(
      _statusPopup, 0, 0, vw, vh/4, 0, null, false, false, HPos.CENTER, VPos.CENTER)
    // the minibuffer overlay is top-aligned at height/4 and extends downward
    if (_mini.isVisible) layoutInArea(
      _mini, 0, vh/4, vw, 3*vh/4, 0, null, false, false, HPos.CENTER, VPos.TOP)
  }

  private def createEmptyBuffer (name :String, args :List[State.Init[_]]) = {
    val parent = _buffers.headOption match {
      case None     => cwd
      case Some(ob) => Paths.get(ob.buffer.store.parent)
    }
    val buf = BufferImpl(Store(parent.resolve(name)))
    args foreach { _.apply(buf.state) }
    buf
  }

  private def freshName (name :String, n :Int = 1) :String = {
    val revName = s"$name<$n>"
    if (_buffers.exists(_.name == revName)) freshName(name, n+1)
    else revName
  }

  private var _pendingMessages :List[String] = null
  private final val MessagesName = "*messages*"
  private def newMessages () = {
    _pendingMessages = Nil
    val mbuf = newBuffer(BufferImpl.scratch(MessagesName), bufferConfig("").mode("log"))
    _pendingMessages foreach { msg =>
      mbuf.view.point() = mbuf.buffer.append(Line.fromText(msg + System.lineSeparator))
    }
    _pendingMessages = null
    mbuf
  }

  private def recordMessage (msg :String) {
    // we may get a recordMessage call while we're creating the *messages* buffer, so to avoid
    // the infinite loop of doom in that case, we buffer messages during that process
    if (_pendingMessages != null) _pendingMessages = msg :: _pendingMessages
    else {
      // create or recreate the *messages* buffer as needed
      val ob = _buffers.find(_.name == MessagesName) getOrElse newMessages()
      ob.view.point() = ob.buffer.append(Line.fromText(msg + System.lineSeparator))
    }
  }

  private final val ScratchName = "*scratch*"
  private def newScratch () = newBuffer(BufferImpl.scratch(ScratchName), bufferConfig(""))

  private def newBuffer (buf :BufferImpl, config :BufferConfig) :OpenBuffer = {
    val (width, height) = size

    // if no mode was specified, have the package manager infer one
    val mode = config._mode getOrElse ws.app.pkgMgr.detectMode(buf.name, buf.lines(0).asString)

    // create the modeline and add some default data before anyone else sneaks in
    val mline = new ModeLineImpl(this)
    mline.addDatum(buf.dirtyV map(if (_) " *" else " -"), "* indicates unsaved changes")
    mline.addDatum(buf.nameV, "Name of the current buffer")

    // TODO: move this to LineNumberMode? (and enable col number therein)
    val view = new BufferViewImpl(this, buf, width, height)
    mline.addDatum(view.point map(p => s" L${p.row+1} C${p.col} "), "Current line number")
    // add "*" to our list of tags as this is a "real" buffer; we want global minor modes, but we
    // don't want non-real buffers (like the minimode buffer) to have global minor modes
    val tags = "*" :: config._tags
    val disp = new DispatcherImpl(this, resolver, view, mline, mode, config._args, tags)

    // TODO: rename this buffer to name<2> (etc.) if its name conflicts with an existing buffer;
    // also set up a listener on it such that if it is written to a new file and that new file has
    // a name that conflicts with an existing buffer, we name<2> it then as well

    val pane = new BorderPane()
    val area = new BufferArea(this, view, disp)
    pane.setCenter(area)
    pane.setBottom(mline)

    val obuf = OpenBuffer(pane, area, view, disp)
    _buffers += obuf
    obuf
  }

  private def requireBuffer (buffer :Buffer) = _buffers.find(_.buffer == buffer) getOrElse {
    throw new IllegalArgumentException(s"Invalid buffer $buffer")
  }

  private def killBuffer (obuf :OpenBuffer) {
    // TODO: run buffer kill hooks to determine if it should be killed
    _buffers -= obuf
    obuf.dispose()

    // if our last buffer is killed, create a new scratch buffer
    if (_buffers.isEmpty) newScratch()
    // otherwise if the killed buffer was focused, display the most recently edited buffer
    else if (_focus() == obuf) _focus() = _buffers.head
  }

  private def onFocusChange (buf :OpenBuffer) {
    setBuffer(buf)
    buf.area.requestFocus()
    // also move the focused buffer to the head of the buffers
    _buffers -= buf
    _buffers prepend buf
  }
}
