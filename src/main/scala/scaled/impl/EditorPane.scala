//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import javafx.application.{Application, Platform}
import javafx.scene.control.{Label, Tab, TabPane}
import javafx.scene.layout.{BorderPane, HBox, Priority, VBox}
import javafx.scene.paint.Color

import reactual.Future

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
class EditorPane (app :Application) extends BorderPane {

  val editor = new Editor {
    override def exit (code :Int) = sys.exit(code) // TODO: cleanup?
    override def showURL (url :String) = app.getHostServices.showDocument(url)
    override val killRing = new KillRingImpl(40) // TODO: get size from config

    override def emitStatus (msg :String) = mini.emitStatus(msg)
    override def clearStatus () = mini.clearStatus()

    override def miniRead (prompt :String, defval :String) = {
      val tab = tabs.getSelectionModel.getSelectedItem
      mini.read(prompt, defval) onComplete { _ =>
        // restore the selected tab (and focus) on read completion
        tabs.getSelectionModel.select(tab)
        deferredFocus(tab.getContent.asInstanceOf[BufferArea])
      }
    }
  }

  val (miniPrompt :Label, mini :Minibuffer.Area) = Minibuffer.create(editor)
  val tabs = new TabPane()
  // TODO: non-placeholder UI for the status line
  val statusLine = new Label("Status: TODO")
  statusLine.getStyleClass.add("status")
  statusLine.setMaxWidth(Double.MaxValue)

  def openTab (file :File) {
    val view = new BufferViewImpl(editor, BufferImpl.fromFile(file), 80, 24)
    // TODO: determine the proper mode based on user customizable mechanism
    val mode = new TextMode(editor, view)
    val area = new BufferArea(editor, view, mode)

    val tab = new Tab()
    tab.setText(file.getName)
    tab.setContent(area)
    // TODO: this doesn't work for mouse based selection; need to implement hacky workaround, or
    // maybe we'll roll our own TabPane since we don't want a lot of the other fiddly business
    tab.selectedProperty.addListener(onChangeB { isSel =>
      if (isSel) deferredFocus(area)
    })
    tabs.getTabs.add(tab)
  }

  setCenter(tabs)
  setBottom({
    val minirow = new HBox(4)
    HBox.setHgrow(mini, Priority.ALWAYS)
    minirow.getChildren.addAll(miniPrompt, mini)
    val vbox = new VBox()
    vbox.setFillWidth(true)
    vbox.getChildren.addAll(statusLine, minirow)
    vbox
  })

  private def deferredFocus (area :BufferArea) {
    Platform.runLater(new Runnable() {
      override def run () = area.requestFocus()
    })
  }
}
