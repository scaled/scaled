//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.scene.text.TextLayout
import com.sun.javafx.tk.Toolkit
import javafx.scene.text.Font
import javafx.scene.text.TextBoundsType
import scala.annotation.tailrec
import scaled._

/** Various utilities used by our controls. */
object Utils {

  class MaxLengthTracker (buffer :RBuffer) {
    def maxLength :Int = buffer.lines(_maxRow).length

    private var _maxRow = longest(buffer.lines, 0, buffer.lines.length)
    private def longest (lines :Seq[LineV], start :Int, end :Int) :Int = {
      @inline @tailrec def loop (cur :Int, max :Int, maxLen :Int) :Int =
        if (cur >= end) max
        else {
          val curLen = lines(cur).length
          if (curLen > maxLen) loop(cur+1, cur, curLen)
          else loop(cur+1, max, maxLen)
        }
      loop(start, 0, 0)
    }

    buffer.edited.onValue { edit => edit match {
      case Buffer.Insert(start, end, _) =>
        // check inserted lines for new longest line
        _maxRow = math.max(_maxRow, longest(buffer.lines, start.row, end.row+1))

      case Buffer.Delete(start, _, _) =>
        // if our old longest line was in the deleted region, rescan buffer for new longest
        if (_maxRow >= start.row && _maxRow <= edit.end.row)
          _maxRow = longest(buffer.lines, 0, buffer.lines.length)

      case _ => // no changes on transform
    }}
  }

  def computeTextWidth (font :Font, text :String) :Double = {
    layout.setContent(if (text != null) text else "", font.impl_getNativeFont)
    layout.getBounds.getWidth
  }

  def getLineHeight (font :Font, boundsType :TextBoundsType) :Double = {
    layout.setContent("", font.impl_getNativeFont())
    layout.setWrapWidth(0)
    layout.setLineSpacing(0)
    if (boundsType == TextBoundsType.LOGICAL_VERTICAL_CENTER) layout.setBoundsType(
      TextLayout.BOUNDS_CENTER);
    else layout.setBoundsType(0)
    layout.getBounds.getHeight
  }

  private val layout = Toolkit.getToolkit.getTextLayoutFactory.createLayout()
}
