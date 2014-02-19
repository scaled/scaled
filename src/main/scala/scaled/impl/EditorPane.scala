//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label
import javafx.scene.control.TextField
import javafx.scene.layout.BorderPane
import javafx.scene.layout.VBox

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
class EditorPane (_buffer :BufferImpl) extends BorderPane {

  val bview = new BufferViewImpl(_buffer)

  // TODO: determine the proper mode based on user customizable mechanism
  val mode = new TextMode(bview)

  val disp = new KeyDispatcher(bview, mode)

  val code = new CodeArea(bview, disp)
  setCenter(code)

  // // TODO: non-placeholder UI for these bits
  // val statusLine = new Label("Status: TODO")
  // val minibuffer = new TextField("")
  // minibuffer.setEditable(false)
  // setBottom({
  //   val vbox = new VBox()
  //   vbox.getChildren.addAll(statusLine, minibuffer)
  //   vbox
  // })
}
