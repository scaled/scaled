//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label

import scaled._

class ModeLine (editor :Editor, view :BufferViewImpl, disp :DispatcherImpl) extends Label {
  getStyleClass.add("modeLine")
  setMaxWidth(Double.MaxValue)

  view.buffer.nameV onEmit update
  view.buffer.dirtyV onEmit update
  view.point onEmit update
  disp.major onEmit update
  update()

  // TODO: break this out into proper UI elements and update them individually?
  private def update () {
    val buf = new StringBuilder(" ")
    buf.append(if (view.buffer.dirty) "*" else "-")
    buf.append(" ").append(view.buffer.name)
    buf.append("  ").append("L").append(view.point().row+1)
    // TODO: col number mode?
    buf.append("  [").append(disp.major().name).append("]")
    // TODO: minor modes (only the ones that want to be reported?)
    setText(buf.toString)
  }
}
