//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.ArrayBuffer

import javafx.application.Application
import javafx.scene.control.Label
import javafx.scene.layout.{BorderPane, Region}
import javafx.stage.Stage

import reactual.Value

import scaled._
import scaled.major.TextMode

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
      _active = buf
      getChildren.add(_active.content)
    }
  }

  private val _minirow = new BorderPane()
  private val _mini :Minibuffer.Area = {
    val (miniPrompt :Label, mini :Minibuffer.Area) = Minibuffer.create(this)
    _minirow.setLeft(miniPrompt)
    _minirow.setCenter(mini)
    mini
  }
  getChildren.add(_minirow)

  // we manage focus specially, via this reactive value
  private val _focus = Value[OpenBuffer](null)
  _focus onValue onFocusChange

  override def exit (code :Int) = sys.exit(code) // TODO: cleanup?
  override def showURL (url :String) = app.getHostServices.showDocument(url)
  override val killRing = new KillRingImpl(40) // TODO: get size from config

  override def emitStatus (msg :String) {
    // we might be asked to emit status while creating the minibuffer, so don't freak out if that's
    // the case; just don't display anything, but add the status to our history (when that exists)
    if (_mini != null) _mini.emitStatus(msg)
  }
  override def clearStatus () = _mini.clearStatus()

  override def miniRead (prompt :String, defval :String, completer :String => Set[String]) = {
    val ofocus = _focus.get // note the current focus
    _focus.update(null)     // focus the minibuffer
    _mini.read(prompt, defval, completer) onComplete { _ =>
      _focus.update(ofocus) // restore the focus on read completion
    }
  }

  override def buffers = _buffers.map(_.buffer)

  override def openBuffer (buffer :String) = _buffers.find(_.name == buffer) match {
    case Some(ob) => _focus.update(ob)
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
    case Some(ob) => _focus.update(ob)
    case None =>
      if (file.exists) newBuffer(BufferImpl.fromFile(file))
      else {
        newBuffer(BufferImpl.empty(file.getName, file))
        emitStatus("(New file)")
      }
  }

  // we manage layout manually so that we can manage the relative z-order of the buffer view versus
  // the minibuffer view (so that popups hover over everything properly)
  override protected def computeMinWidth (height :Double) = _active.content.minWidth(-1)
  override protected def computeMinHeight (height :Double) =
    _active.content.minHeight(-1) + _minirow.minHeight(-1)
  override protected def computePrefWidth (height :Double) = _active.content.prefWidth(-1)
  override protected def computePrefHeight (width :Double) =
    _active.content.prefHeight(-1) + _minirow.prefHeight(-1)
  override protected def computeMaxWidth (height :Double) = Double.MaxValue
  override protected def computeMaxHeight (width :Double) = Double.MaxValue
  override def layoutChildren () {
    val pw = _active.content.prefWidth(-1); val ph = _active.content.prefHeight(-1)
    _active.content.resize(pw, ph)
    _minirow.resize(pw, _minirow.prefHeight(-1))
    _minirow.setLayoutY(ph)
  }

  private def newBuffer (buf :BufferImpl) {
    val view = new BufferViewImpl(this, buf, 80, 24)
    // TODO: determine the proper mode based on user customizable mechanism
    val disp = new DispatcherImpl(this, view) {
      override def createMode () = new TextMode(EditorPane.this, view, this)
    }

    val content = new BorderPane()
    val area = new BufferArea(this, view, disp)
    content.setCenter(area)
    content.setBottom(new ModeLine(this, view))

    val obuf = OpenBuffer(content, area, view)
    _buffers prepend obuf
    _focus.update(obuf)
  }

  private def killBuffer (obuf :OpenBuffer) {
    // TODO: run buffer kill hooks to determine if it should be killed
    _buffers -= obuf

    // if our last buffer is killed, create a new scratch buffer
    if (_buffers.isEmpty) newBuffer(BufferImpl.scratch("*scratch*"))
    // otherwise if the killed buffer was focused, display the most recently edited buffer
    else if (_focus.get == obuf) _focus.update(_buffers.head)
  }

  private def onFocusChange (buf :OpenBuffer) {
    if (buf == null) {
      _mini.requestFocus()
      _minirow.toFront()
    } else {
      setBuffer(buf)
      buf.area.requestFocus()
      // also move the focused buffer to the head of the buffers
      _buffers -= buf
      _buffers prepend buf
    }
  }
}
