//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.ArrayBuffer

import javafx.application.Application
import javafx.scene.control.Label
import javafx.scene.layout.BorderPane
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
class EditorPane (app :Application, stage :Stage) extends BorderPane with Editor {

  private case class OpenBuffer (content :BorderPane, area :BufferArea, view :BufferViewImpl) {
    def buffer = view.buffer
    def name = view.buffer.name
    override def toString = name
  }
  private val _buffers = ArrayBuffer[OpenBuffer]()

  private val _mini :Minibuffer.Area = {
    val (miniPrompt :Label, mini :Minibuffer.Area) = Minibuffer.create(this)
    setBottom({
      val minirow = new BorderPane()
      minirow.setLeft(miniPrompt)
      minirow.setCenter(mini)
      minirow
    })
    mini
  }

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

  override def newBuffer (file :File) = newBuffer(BufferImpl.fromFile(file))

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
      _mini.toFront()
    }
    else {
      setCenter(buf.content)
      buf.content.toFront()
      buf.area.requestFocus()
      // also move the focused buffer to the head of the buffers
      _buffers -= buf
      _buffers prepend buf
    }
  }
}
