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
import javafx.scene.text.{Text, TextFlow, FontSmoothingType}

import scaled._

class LineViewImpl (_line :LineV) extends LineView {

  override def line = _line
  private var _valid = false

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

  /** Returns the x position of character at the specified column.
    * @param charWidth the current width of the (fixed) view font.
    */
  def charX (col :Int, charWidth :Double) :Double = {
    // TODO: handle tabs, other funny business?
    node.getLayoutX + col*charWidth
  }

  /** Updates this line to reflect the supplied style change. */
  def onStyle (loc :Loc) {
    // TODO: only change the spans that are affected by the restyle
    invalidate()
  }

  /** Updates this line's visibility. Lines outside the visible area of the buffer are marked
    * non-visible and defer applying line changes until they are once again visible. */
  def setVisible (viz :Boolean) :Boolean = {
    val validated = if (viz && !_valid) { validate() ; true }
                    else false
    node.setVisible(viz)
    validated
  }

  /** Marks this line view as invalid, clearing its children. */
  def invalidate () :Unit = if (_valid) {
    _valid = false
    node.getChildren.clear()
    if (node.isVisible) validate()
  }

  /** Validates this line, rebuilding its visualization. This is called when the line becomes
    * visible. Non-visible lines defer visualization rebuilds until they become visible. */
  def validate () :Unit = if (!_valid) {
    _valid = true
    // go through the line accumulating runs of characters that are all styled with the same face
    val last = _line.length
    var start = 0
    var end = 0
    var styles :Styles = null
    val kids = ArrayBuffer[Node]()
    while (end <= last) {
      val cstyles = _line.stylesAt(end)
      if ((cstyles ne styles) || end == last) {
        if (end > 0) {
          val text = _line.sliceString(start, end)
          assert(end > start)
          assert(text.indexOf('\r') == -1 && text.indexOf('\n') == -1,
                 s"Text cannot have newlines: $text")
          val tnode = new FillableText(text)
          tnode.setFontSmoothingType(FontSmoothingType.LCD)
          tnode.getStyleClass.add("textFace")
          styles.addTo(tnode.getStyleClass)
          tnode.setTextOrigin(VPos.TOP)
          kids += tnode.fillRect
          kids += tnode
        }
        styles = cstyles
        start = end
      }
      end += 1
    }

    if (!kids.isEmpty) node.getChildren.addAll(kids.toArray :_*)
  }
}
