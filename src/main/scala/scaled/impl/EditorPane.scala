//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.ArrayBuffer

import javafx.application.{Application, Platform}
import javafx.event.{Event, EventHandler}
import javafx.scene.Node
import javafx.scene.control.{Label, Tab, TabPane}
import javafx.scene.layout.{BorderPane, HBox, Priority, VBox}
import javafx.scene.paint.Color
import javafx.stage.Stage

import reactual.{Future, Value}

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

  private case class OpenBuffer (tab :Tab, view :BufferViewImpl) {
    def buffer = view.buffer
    def name = view.buffer.name
  }
  private val _buffers = ArrayBuffer[OpenBuffer]()

  private val _tabs = new TabPane()

  private val _mini :Minibuffer.Area = {
    val (miniPrompt :Label, mini :Minibuffer.Area) = Minibuffer.create(this)
    // TODO: non-placeholder UI for the status line
    val statusLine = new Label("Status: TODO")
    statusLine.getStyleClass.add("status")
    statusLine.setMaxWidth(Double.MaxValue)

    setCenter(_tabs)
    setBottom({
      val bits = new BorderPane()
      bits.setTop(statusLine)
      bits.setLeft(miniPrompt)
      bits.setCenter(mini)
      bits
    })
    mini
  }

  // we manage focus specially, via this reactive value
  private val _focus = Value[OpenBuffer](null)
  _focus onValue onFocusChange

  override def exit (code :Int) = sys.exit(code) // TODO: cleanup?
  override def showURL (url :String) = app.getHostServices.showDocument(url)
  override val killRing = new KillRingImpl(40) // TODO: get size from config

  override def emitStatus (msg :String) = _mini.emitStatus(msg)
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
    case Some(ob) => _focus.update(ob) ; true
    case None     => false
  }

  override def killBuffer (buffer :String) = _buffers.find(_.name == buffer) match {
    case Some(ob) => killBuffer(ob) ; true
    case None     => false
  }

  override def newBuffer (file :File) {
    val view = new BufferViewImpl(this, BufferImpl.fromFile(file), 80, 24)
    // TODO: determine the proper mode based on user customizable mechanism
    val disp = new DispatcherImpl(this, view) {
      override def createMode () = new TextMode(EditorPane.this, view, this)
    }
    val area = new BufferArea(this, view, disp)

    val tab = new Tab()
    val obuf = OpenBuffer(tab, view)
    tab.setOnCloseRequest(new EventHandler[Event]() {
      def handle (ev :Event) = { killBuffer(obuf) ; ev.consume() }
    })
    // TODO: if this tab is closed via the UI, remove our OB from _buffers
    view.buffer.nameV onValueNotify tab.setText
    tab.setContent(area)
    // TODO: this doesn't work for mouse based selection; need to implement hacky workaround, or
    // maybe we'll roll our own TabPane since we don't want a lot of the other fiddly business
    tab.selectedProperty.addListener(onChangeB { isSel =>
      if (isSel) _focus.update(obuf)
    })
    _tabs.getTabs.add(tab)
    _buffers prepend obuf
  }

  private def killBuffer (obuf :OpenBuffer) {
    // TODO: run buffer kill hooks to determine if it should be killed
    _buffers -= obuf
    _tabs.getTabs.remove(obuf.tab)

    // TODO: if our last buffer is killed, add a scratch buffer?
  }

  private def onFocusChange (buf :OpenBuffer) {
    if (buf == null) _mini.requestFocus()
    else {
      // make sure the focused buffer's tab is visible and has JavaFX focus (note: we can't request
      // focus immediately, we have to defer it to the next FX UI tick)
      if (_tabs.getSelectionModel.getSelectedItem == buf.tab) defer {
        // focus may have changed while we were being deferred, so double check
        if (_focus.get == buf) buf.tab.getContent.requestFocus()
      }
      else _tabs.getSelectionModel.select(buf.tab)
      // also move the focused buffer to the head of the buffers
      _buffers -= buf
      _buffers prepend buf
    }
  }
}
