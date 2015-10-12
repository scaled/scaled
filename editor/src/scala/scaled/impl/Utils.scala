//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.scene.text.TextLayout
import com.sun.javafx.tk.Toolkit
import java.io.{StringWriter, PrintWriter}
import javafx.scene.text.Font
import javafx.scene.text.TextBoundsType
import scaled._

/** Various utilities used by the Scaled implementation code. */
object Utils {

  def computeTextWidth (font :Font, text :String) :Double = {
    layout.setContent(if (text != null) text else "", Dep.getNativeFont(font))
    layout.getBounds.getWidth
  }

  def getLineHeight (font :Font, boundsType :TextBoundsType) :Double = {
    layout.setContent("", Dep.getNativeFont(font))
    layout.setWrapWidth(0)
    layout.setLineSpacing(0)
    if (boundsType == TextBoundsType.LOGICAL_VERTICAL_CENTER) layout.setBoundsType(
      TextLayout.BOUNDS_CENTER);
    else layout.setBoundsType(0)
    layout.getBounds.getHeight
  }

  def safeSignal[T] (log :Logger) :Signal[T] = new Signal[T]() {
    override def emit (value :T) :Unit = try {
      super.emit(value)
    } catch {
      case t :Throwable => log.log(s"Signal.emit failure [value=$value]", t)
    }
  }

  private val layout = Toolkit.getToolkit.getTextLayoutFactory.createLayout()
}
