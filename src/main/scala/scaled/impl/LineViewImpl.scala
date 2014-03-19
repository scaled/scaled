//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import javafx.geometry.VPos
import javafx.scene.shape.Rectangle
import javafx.scene.text.Text
import javafx.scene.text.TextFlow

import scaled._

class LineViewImpl (_line :LineV) extends LineView {

  override def line = _line

  val spans = ArrayBuffer[Span]()
  class Span (start :Int, end :Int, face :Face) {
    val text = _line.sliceString(start, end)
    assert(!text.contains('\r') && !text.contains('\n'))

    // displaying background requires rendering a separate filled rectangle behind our text
    val rect = if (!face.background.isDefined) null else {
      val r = new Rectangle(1, 1, face.background.get)
      r.setManaged(false)
      node.getChildren.add(r)
      r
    }

    val tnode = new Text(text)
    tnode.getStyleClass.add("text")
    tnode.setTextOrigin(VPos.TOP)
    if (face.foreground.isDefined) tnode.setFill(face.foreground.get)
    // TODO: font variants
    node.getChildren.add(tnode)

    def layout () {
      if (rect != null) {
        // size and position our bg rect based on the size and position of our text
        val bounds = tnode.getLayoutBounds
        rect.setWidth(bounds.getWidth)
        rect.setHeight(bounds.getHeight+1) // see BufferArea.lineHeight for hackery excuse (TODO)
        rect.relocate(tnode.getLayoutX, tnode.getLayoutY)
      }
    }
  }

  val node = new TextFlow() {
    override def layoutChildren () {
      super.layoutChildren()
      spans foreach(_.layout())
    }
  }
  node.setManaged(false)
  // node.fontProperty.bind(ctrl.fontProperty)
  // node.fillProperty.bind(textFill)
  // node.impl_selectionFillProperty().bind(highlightTextFill)
  rebuild()

  /** Returns the x position of character at the specified column.
    * @param charWidth the current width of the (fixed) view font.
    */
  def charX (col :Int, charWidth :Double) :Double = {
    // TODO: handle tabs, other funny business?
    node.getLayoutX + col*charWidth
  }

  /** Updates this line to reflect the supplied edit. */
  def onEdit (change :Line.Edit) {
    // TODO: only change the spans that are affected by the edit
    rebuild()
  }

  /** Updates this line to reflect the supplied style change. */
  def onStyle (loc :Loc) {
    // TODO: only change the spans that are affected by the restyle
    rebuild()
  }

  def rebuild () {
    node.getChildren.clear()
    spans.clear()

    // go through the line accumulating runs of characters that are all styled with the same face
    val last = _line.length
    var start = 0
    var end = 0
    var face :Face = null
    while (end <= last) {
      val cface = _line.faceAt(end)
      if (cface != face || end == last) {
        if (end > 0) spans += new Span(start, end, face)
        face = cface
        start = end
      }
      end += 1
    }
  }
}
