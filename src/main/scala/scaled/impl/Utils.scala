//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.scene.text.TextLayout
import com.sun.javafx.tk.Toolkit

import javafx.scene.text.Font
import javafx.scene.text.TextBoundsType

import scaled._

/** Various utilities used by our controls. */
object Utils {

  class MaxLengthTracker (buffer :RBuffer) {
    def maxLength :Int = _maxLength
    private var _maxLength = buffer.lines.map(_.length).max
    buffer.lineEdited.onValue { edit =>
      val nlen = edit.addedLine.length
      // if this line is longer than our current max length, increase it
      if (nlen > _maxLength) _maxLength = nlen
      // otherwise if this line's old length was our max length,
      // we need to rescan the buffer to find the new longest line
      else {
        val olen = nlen - edit.added + edit.deleted
        if (olen >= _maxLength) _maxLength = buffer.lines.map(_.length).max
      }
    }
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
