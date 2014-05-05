//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.scene.text.TextLayout
import com.sun.javafx.tk.Toolkit
import java.io.{StringWriter, PrintWriter}
import javafx.scene.text.Font
import javafx.scene.text.TextBoundsType
import scala.annotation.tailrec
import scaled._

/** Various utilities used by our controls. */
object Utils {

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

  def stackTraceToString (exn :Throwable) :String = {
    val trace = new StringWriter()
    exn.printStackTrace(new PrintWriter(trace))
    trace.toString
  }

  private val layout = Toolkit.getToolkit.getTextLayoutFactory.createLayout()
}
