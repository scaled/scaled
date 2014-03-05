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
}
