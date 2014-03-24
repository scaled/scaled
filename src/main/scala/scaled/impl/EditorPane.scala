//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.ArrayBuffer

import javafx.application.{Application, Platform}
import javafx.geometry.{HPos, VPos}
import javafx.scene.control.Label
import javafx.scene.layout.{BorderPane, Region}
import javafx.stage.Stage

import reactual.{Future, Promise, Value}

import scaled._
import scaled.major.TextMode
import scaled.minor.WhitespaceMode

/** The editor pane groups together the various UI components that are needed to edit a single
  * buffer. This includes the code area, status line and minibuffer area. It also manages the
  * [[BufferView]] state and wires everything together.
  *
  * Multiple instances of an editor pane may be instantiated at the same time (and placed into tabs
  * or simply shown one at a time, depending on the user's configuration), but each editor pane is
  * largely an island unto itself.
  */
class EditorPane (app :Application, stage :Stage) extends Region with Editor {

  private case class OpenBuffer (content :BorderPane, area :BufferArea, view :BufferViewImpl) {
    def buffer = view.buffer
    def name = view.buffer.name
    override def toString = name
  }
  private val _buffers = ArrayBuffer[OpenBuffer]()

  private var _active :OpenBuffer = _
  private def setBuffer (buf :OpenBuffer) {
    if (_active == buf) _active.content.toFront()
    else {
      if (_active != null) getChildren.remove(_active.content)
      stage.setTitle(s"Scaled - ${buf.name}")
      _active = buf
      getChildren.add(_active.content)
    }
  }

  private val _status = new Label()
  _status.maxWidthProperty.bind(widthProperty)
  _status.setWrapText(true)
  _status.getStyleClass.addAll("overpop", "status")
  getChildren.add(_status)

  private val _mini = new MiniOverlay(this) {
    override def onClear () = _focus().area.requestFocus() // restore buffer focus on clear
  }
  getChildren.add(_mini)

  // we manage focus specially, via this reactive value
  private val _focus = Value[OpenBuffer](null)
  _focus onValue onFocusChange

  newScratch() // always start with a scratch buffer

  /** Used to resolve modes in this editor. */
  val resolver = new ModeResolver(this)

  /** The global editor configuration. */
  def config = new ConfigImpl()

  override def exit (code :Int) = sys.exit(code) // TODO: cleanup?
  override def showURL (url :String) = app.getHostServices.showDocument(url)
  override def defer (op : => Unit) = Platform.runLater(new Runnable() {
    override def run () = op
  })
  override val killRing = new KillRingImpl(config(EditorConfig.killRingSize))

  override def emitStatus (msg :String) {
    _status.toFront()
    _status.setText(msg) // TODO: fade in
    _status.setVisible(true)
  }
  override def clearStatus () = {
    if (_status.isVisible()) {
      _status.setText("") // TODO: fade out
      _status.setVisible(false)
    }
  }

  override def mini[R] (mode :String, result :Promise[R], args :Any*) :Future[R] = {
    _mini.toFront()
    _mini.read(mode, result, args.toList)
  }

  override def buffers = _buffers.map(_.buffer)

  override def openBuffer (buffer :String) = _buffers.find(_.name == buffer) match {
    case Some(ob) => _focus() = ob
    // if we find no buffer, create a new one with the specified name
    case None =>
      val file = _buffers.headOption map(_.buffer.dir) getOrElse(cwd())
      newBuffer(BufferImpl.empty(buffer, file))
  }

  override def killBuffer (buffer :String) = _buffers.find(_.name == buffer) match {
    case Some(ob) => killBuffer(ob) ; true
    case None     => false
  }

  // if another buffer exists that is visiting this file, just open it
  override def newBuffer (file :File) = _buffers.find(_.buffer.file == file) match {
    case Some(ob) => _focus() = ob
    case None =>
      if (file.exists) newBuffer(BufferImpl.fromFile(file))
      else {
        newBuffer(BufferImpl.empty(file.getName, file))
        emitStatus("(New file)")
      }
  }

  // we manage layout manually for a variety of nefarious reasons
  override protected def computeMinWidth (height :Double) = _active.content.minWidth(height)
  override protected def computeMinHeight (width :Double) = _active.content.minHeight(width)
  override protected def computePrefWidth (height :Double) = _active.content.prefWidth(height)
  override protected def computePrefHeight (width :Double) = _active.content.prefHeight(width)
  override protected def computeMaxWidth (height :Double) = Double.MaxValue
  override protected def computeMaxHeight (width :Double) = Double.MaxValue

  override def layoutChildren () {
    val bounds = getLayoutBounds
    _active.content.resize(bounds.getWidth, bounds.getHeight)

    val vw = bounds.getWidth ; val vh = bounds.getHeight
    // the status overlay is centered in the top 1/4th of the screen
    if (_status.isVisible) layoutInArea(
      _status, 0, 0, vw, vh/4, 0, null, false, false, HPos.CENTER, VPos.CENTER)
    // the minibuffer overlay is top-aligned at height/4 and extends downward
    if (_mini.isVisible) layoutInArea(
      _mini, 0, vh/4, vw, 3*vh/4, 0, null, false, false, HPos.CENTER, VPos.TOP)
  }

  private def newScratch () = newBuffer(BufferImpl.scratch("*scratch*"))

  private def newBuffer (buf :BufferImpl) {
    val config = new ConfigImpl() // TODO: inherit from global editor config?
    val view = new BufferViewImpl(
      this, buf, config(EditorConfig.viewWidth), config(EditorConfig.viewHeight))
    // TODO: determine the proper mode based on user customizable mechanism
    val disp = new DispatcherImpl(this, view) {
      override def createMode () = new TextMode(EditorPane.this, config, view, this)
    }

    // TEMP: hack in whitespace mode activation for testing for now
    disp.addMode(new WhitespaceMode(EditorPane.this, config, view,
                                    disp.major.asInstanceOf[TextMode]))

    // TODO: rename this buffer to name<2> (etc.) if its name conflicts with an existing buffer;
    // also set up a listener on it such that if it is written to a new file and that new file has
    // a name that conflicts with an existing buffer, we name<2> it then as well

    val content = new BorderPane()
    val area = new BufferArea(this, view, disp)
    content.setCenter(area)
    content.setBottom(new ModeLine(this, view))

    val obuf = OpenBuffer(content, area, view)
    _buffers prepend obuf
    _focus() = obuf
  }

  private def killBuffer (obuf :OpenBuffer) {
    // TODO: run buffer kill hooks to determine if it should be killed
    _buffers -= obuf

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
