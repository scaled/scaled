//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.css.converters.PaintConverter

import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.css.{CssMetaData, Styleable, StyleableObjectProperty}
import javafx.geometry.Bounds
import javafx.scene.paint.{Color, Paint}
import javafx.scene.shape.Rectangle
import javafx.scene.text.Text

class FillableText (text :String) extends Text(text) {

  /** The stroke to use when rendering this text's background */
  val bgStroke = new StyleableObjectProperty[Paint](null) {
    override def invalidated () {} // nada
    override def getBean = FillableText.this
    override def getName = "bgStroke"
    override def getCssMetaData = FillableText.BgStroke
  }

  /** The fill to use when rendering this text's background */
  val bgFill = new StyleableObjectProperty[Paint](null) {
    override def invalidated () {} // nada
    override def getBean = FillableText.this
    override def getName = "bgFill"
    override def getCssMetaData = FillableText.BgFill
  }

  val fillRect = new Rectangle()
  fillRect.setManaged(false)
  fillRect.setStroke(null)
  fillRect.layoutXProperty.bind(layoutXProperty)
  fillRect.strokeProperty().bind(bgStroke)
  fillRect.fillProperty().bind(bgFill)

  // fill rect visibility depends on our viz and whether we have a bg fill
  visibleProperty.addListener(onChangeB { viz => updateFillViz() })
  bgStroke.addListener(onChange[Paint] { nstroke => updateFillViz() })
  bgFill.addListener(onChange[Paint] { nfill => updateFillViz() })
  private def updateFillViz () = fillRect.setVisible(
    isVisible && (bgFill.getValue != null || bgStroke.getValue != null))

  // we have to do this layout syncing manually (via LineViewImpl), because binding to layoutBounds
  // causes weird JavaFX internal failures and there's no public or protected hook we can tie into
  // during the TextFlow/Text layout process
  def layoutRect () {
    val bounds = getLayoutBounds
    fillRect.setWidth(bounds.getWidth)
    fillRect.setHeight(bounds.getHeight+1) // TODO: see EditorPane height hack
  }

  override def getCssMetaData = FillableText.classCssMetaData
}

object FillableText {
  import java.util.{ArrayList, Collections, List => JList}

  val BgStroke :CssMetaData[FillableText,Paint] = new CssMetaData[FillableText,Paint](
    "-fx-bg-stroke", PaintConverter.getInstance(), null) {
    override def isSettable (n :FillableText) = true
    override def getStyleableProperty (n :FillableText) = n.bgStroke
  }

  val BgFill :CssMetaData[FillableText,Paint] = new CssMetaData[FillableText,Paint](
    "-fx-bg-fill", PaintConverter.getInstance(), null) {
    override def isSettable (n :FillableText) = true
    override def getStyleableProperty (n :FillableText) = n.bgFill
  }

  val classCssMetaData :JList[CssMetaData[_ <: Styleable, _]] = {
    val styleables = new ArrayList(Text.getClassCssMetaData())
    styleables.add(BgStroke)
    styleables.add(BgFill)
    Collections.unmodifiableList(styleables)
  }
}
