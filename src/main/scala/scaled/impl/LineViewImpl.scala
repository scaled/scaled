//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import javafx.geometry.VPos
import javafx.scene.Node
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.scene.text.Text
import javafx.scene.text.TextFlow

import scaled._

class LineViewImpl (_line :LineV) extends LineView {

  override def line = _line

  val node = new TextFlow() {
    override def layoutChildren () {
      super.layoutChildren()
      val cs = getChildren
      @tailrec def layout (ii :Int) {
        if (ii >= 0) {
          cs.get(ii) match {
            case ft :FillableText => ft.layoutRect()
            case _ => // nada
          }
          layout(ii-1)
        }
      }
      layout(cs.size-1)
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
    // go through the line accumulating runs of characters that are all styled with the same face
    val last = _line.length
    var start = 0
    var end = 0
    var style :String = null
    val kids = ArrayBuffer[Node]()
    while (end <= last) {
      val cstyle = _line.styleAt(end)
      if (cstyle != style || end == last) {
        if (end > 0) {
          val text = _line.sliceString(start, end)
          assert(end > start)
          assert(!text.contains('\r') && !text.contains('\n'))
          val tnode = new FillableText(text)
          tnode.getStyleClass.add(style)
          tnode.setTextOrigin(VPos.TOP)
          kids += tnode.fillRect
          kids += tnode
        }
        style = cstyle
        start = end
      }
      end += 1
    }

    node.getChildren.clear()
    if (!kids.isEmpty) node.getChildren.addAll(kids.toArray :_*)
  }
}
