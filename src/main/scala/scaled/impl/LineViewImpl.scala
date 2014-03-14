//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.geometry.VPos
import javafx.scene.text.Text

import scaled._

class LineViewImpl (_line :LineV) extends LineView {

  override def line = _line

  val node = new Text(line.asString)
  node.setTextOrigin(VPos.TOP)
  node.setManaged(false)
  node.getStyleClass.add("text")
  // node.fontProperty.bind(ctrl.fontProperty)
  // node.fillProperty.bind(textFill)
  // node.impl_selectionFillProperty().bind(highlightTextFill)

  /** Updates the text displayed by this line. */
  def setText (text :String) {
    // TODO: handle style runs, highlighted text, and who knows what other manner of complexity
    node.setText(text)
  }

  /** Returns the x position of character at the specified column.
    * @param charWidth the current width of the (fixed) view font.
    */
  def charX (col :Int, charWidth :Double) :Double = {
    // TODO: handle tabs, other funny business?
    node.getLayoutX + col*charWidth
  }
}
