//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.scene.control

import javafx.beans.binding.ObjectBinding
import javafx.beans.property.{DoubleProperty, ObjectProperty, SimpleDoubleProperty}
import javafx.beans.value.{ChangeListener, ObservableObjectValue, ObservableValue}
import javafx.beans.{InvalidationListener, Observable}
import javafx.css.{CssMetaData, FontCssMetaData, Styleable, StyleableProperty, StyleOrigin,
  StyleableObjectProperty}
import javafx.event.EventHandler
import javafx.geometry.{Bounds, Rectangle2D, VPos}
import javafx.scene.Group
import javafx.scene.control.{Control, ScrollPane}
import javafx.scene.input.{MouseEvent, KeyCode, KeyEvent}
import javafx.scene.layout.Region
import javafx.scene.text.{Font, Text}

import com.sun.javafx.scene.control.behavior.BehaviorBase
import com.sun.javafx.scene.control.skin.BehaviorSkinBase
import com.sun.javafx.tk.{FontMetrics, Toolkit}

import scala.collection.JavaConversions._

import scaled.buffer.{Buffer, Line}

// TODO
//
// - support passing in start and end anchors and only displaying the part of the buffer between
// those anchors
//
// - how will we support styles? attribute the buffer? probably so because that will cause all
// views of the buffer to remain in sync as attributes are provided by external intelligence

/** A control that displays code and allows it to be edited. A control is instantiated with a
  * [Buffer] and always displays/edits that buffer. You cannot switch the buffer in a CodeArea, you
  * would instead make a new CodeArea.
  */
class CodeArea (val buffer :Buffer) extends Control {

  val  scrollTop :DoubleProperty = new SimpleDoubleProperty(this, "scrollTop", 0)
  val scrollLeft :DoubleProperty = new SimpleDoubleProperty(this, "scrollLeft", 0)

  val font :StyleableObjectProperty[Font] = new StyleableObjectProperty[Font](Font.getDefault()) {
    private var fontSetByCss = false

    override def applyStyle (newOrigin :StyleOrigin, value :Font) {
      // if CSS is setting the font, then make sure invalidate doesn't call impl_reapplyCSS
      try {
        // super.applyStyle calls set which might throw if value is bound. Have to make sure
        // fontSetByCss is reset.
        fontSetByCss = true
        super.applyStyle(newOrigin, value)
      } catch {
        case e :Exception => throw e // TODO: wtf?
      } finally {
        fontSetByCss = false;
      }
    }

    override def set (value :Font) {
      if (value != get()) super.set(value)
    }

    override def getCssMetaData = CodeArea.StyleableProperties.FONT
    override def getBean = CodeArea.this
    override def getName = "font"

    override protected def invalidated () {
      // if font is changed by calling setFont, then css might need to be reapplied since font size
      // affects calculated values for styles with relative values
      if (!fontSetByCss) impl_reapplyCSS()
    }
  }

  override def computeMaxWidth (height :Double) = Double.MaxValue
  override def computeMaxHeight (width :Double) = Double.MaxValue

  override def createDefaultSkin = new CodeArea.Skin(this)
  override def getControlCssMetaData = CodeArea.getClassCssMetaData
}

/** [CodeArea] helper classes and whatnot. */
object CodeArea {

  class Behavior (ctrl :CodeArea) extends BehaviorBase(ctrl, null) {
    override def callActionForEvent (e :KeyEvent) {
      if (e.getEventType == KeyEvent.KEY_PRESSED) {
        e.getCode match {
          case KeyCode.DOWN =>
            ctrl.scrollTop.set(ctrl.scrollTop.get+ctrl.getSkin.asInstanceOf[Skin].lineHeight)
          case KeyCode.UP =>
            ctrl.scrollTop.set(ctrl.scrollTop.get-ctrl.getSkin.asInstanceOf[Skin].lineHeight)
          case _ =>
            println(s"Pressed $e")
            super.callActionForEvent(e)
        }
      } else super.callActionForEvent(e)
    }
  }

  class Skin (ctrl :CodeArea) extends BehaviorSkinBase[CodeArea,Behavior](ctrl, new Behavior(ctrl)) {

    private var computedMinWidth = Double.NegativeInfinity
    private var computedMinHeight = Double.NegativeInfinity
    private var computedPrefWidth = Double.NegativeInfinity
    private var computedPrefHeight = Double.NegativeInfinity
    private var characterWidth = 0d
    var lineHeight = 0d // TEMP viz for scrolling hack

    // ctrl.prefColumnCountProperty.addListener(new ChangeListener<Number>() {
    //   @Override
    //     public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
    //     invalidateMetrics();
    //     updatePrefViewportWidth();
    //   }
    // });
    // ctrl.prefRowCountProperty.addListener(new ChangeListener<Number>() {
    //   @Override
    //     public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
    //     invalidateMetrics();
    //     updatePrefViewportHeight();
    //   }
    // });

    protected val fontMetrics = new ObjectBinding[FontMetrics]() {
      { bind(ctrl.font) }
      override def computeValue = {
        invalidateMetrics()
        Toolkit.getToolkit.getFontLoader.getFontMetrics(ctrl.font.get)
      }
    }
    fontMetrics.addListener(new InvalidationListener() {
      override def invalidated(valueModel :Observable) {
        updateFontMetrics()
      }
    })

    private def updateFontMetrics () {
      val firstLine = lineNodes.getChildren.get(0).asInstanceOf[Text]
      lineHeight = Utils.getLineHeight(getSkinnable.font.get, firstLine.getBoundsType)
      characterWidth = fontMetrics.get.computeStringWidth("W")
    }

    ctrl.scrollTop.addListener(new ChangeListener[Number]() {
      override def changed(observable :ObservableValue[_ <: Number], ov :Number, nv :Number) {
        val vv = if (nv.doubleValue < getScrollTopMax) nv.doubleValue/getScrollTopMax else  1.0
        scrollPane.setVvalue(vv)
      }
    });

    ctrl.scrollLeft.addListener(new ChangeListener[Number]() {
      override def changed(observable :ObservableValue[_ <: Number], ov :Number, nv :Number) {
        val hv = if (nv.doubleValue < getScrollLeftMax) nv.doubleValue/getScrollLeftMax else  1.0
        scrollPane.setHvalue(hv)
      }
    });

    // TODO: listen for buffer changes and create/update/destroy line nodes as appropriate
    // if (USE_MULTIPLE_NODES) {
    //   textArea.getParagraphs().addListener(new ListChangeListener<CharSequence>() {
    //     @Override
    //       public void onChanged(ListChangeListener.Change<? extends CharSequence> change) {
    //       while (change.next()) {
    //         int from = change.getFrom();
    //         int to = change.getTo();
    //         List<? extends CharSequence> removed = change.getRemoved();
    //         if (from < to) {

    //           if (removed.isEmpty()) {
    //             // This is an add
    //             for (int i = from, n = to; i < n; i++) {
    //               addParagraphNode(i, change.getList().get(i).toString());
    //             }
    //           } else {
    //             // This is an update
    //             for (int i = from, n = to; i < n; i++) {
    //               Node node = paragraphNodes.getChildren().get(i);
    //               Text paragraphNode = (Text) node;
    //               paragraphNode.setText(change.getList().get(i).toString());
    //             }
    //           }
    //         } else {
    //           // This is a remove
    //           paragraphNodes.getChildren().subList(from, from + removed.size()).clear();
    //         }
    //       }
    //     }
    //   });
    // } else {
    //   textArea.textProperty().addListener(new InvalidationListener() {
    //     @Override public void invalidated(Observable observable) {
    //       invalidateMetrics();
    //       ((Text)paragraphNodes.getChildren().get(0)).setText(textArea.textProperty().getValueSafe());
    //       contentView.requestLayout();
    //     }
    //   });
    // }

    // contains our content node and scrolls and clips it
    private val scrollPane = new ScrollPane()
    // scrollPane.setStyle("-fx-background-color:blue; -fx-border-color:crimson;") // TEMP
    scrollPane.setMinWidth(0)
    scrollPane.setMinHeight(0)
    // bind the h/vvalue scroll pane props to the scroll left/top code area props
    scrollPane.hvalueProperty().addListener(new ChangeListener[Number]() {
      override def changed (observable :ObservableValue[_ <: Number], ov :Number, nv :Number) {
        getSkinnable.scrollLeft.setValue(nv.doubleValue * getScrollLeftMax)
      }
    })
    scrollPane.vvalueProperty().addListener(new ChangeListener[Number]() {
      override def changed (observable :ObservableValue[_ <: Number], ov :Number, nv :Number) {
        getSkinnable.scrollTop.setValue(nv.doubleValue * getScrollTopMax)
      }
    })

    scrollPane.viewportBoundsProperty.addListener(new InvalidationListener() {
      override def invalidated (valueModel :Observable) {
        if (scrollPane.getViewportBounds != null) {
          // ScrollPane creates a new Bounds instance for each layout pass, so we need to check if
          // the width/height have really changed to avoid infinite layout requests.
          val newViewportBounds = scrollPane.getViewportBounds
          if (oldViewportBounds == null ||
              oldViewportBounds.getWidth() != newViewportBounds.getWidth() ||
              oldViewportBounds.getHeight() != newViewportBounds.getHeight()) {
            invalidateMetrics()
            oldViewportBounds = newViewportBounds
            contentNode.requestLayout()
          }
        }
      }
      private var oldViewportBounds :Bounds = _
    })

    // contains the Text nodes for each line
    private val lineNodes = new Group()
    lineNodes.setManaged(false)

    // contains our line nodes and other decorative nodes (caret, selection, etc.)
    class ContentNode extends Region {
      /*ctor*/ {
        getStyleClass().add("content")
        // forward mouse events to the skin's behavior
        addEventHandler(MouseEvent.MOUSE_PRESSED, new EventHandler[MouseEvent]() {
          override def handle (event :MouseEvent) {
            getBehavior.mousePressed(event)
            event.consume()
          }
        })
        addEventHandler(MouseEvent.MOUSE_RELEASED, new EventHandler[MouseEvent]() {
          override def handle (event :MouseEvent) {
            getBehavior.mouseReleased(event)
            event.consume()
          }
        })
        addEventHandler(MouseEvent.MOUSE_DRAGGED, new EventHandler[MouseEvent]() {
          override def handle (event :MouseEvent) {
            getBehavior.mouseDragged(event)
            event.consume()
          }
        })
      }

      override def resize (w :Double, h :Double) {
        super.resize(w, h)
        println(s"Content resized $w x $h")
      }

      // make this visible
      override def getChildren = super.getChildren

      override protected def computePrefWidth (height :Double) = {
        if (computedPrefWidth < 0) {
          val maxWidth = lineNodes.getChildren.map(_.asInstanceOf[Text]).map(
            t => Utils.computeTextWidth(t.getFont, t.getText)).max
          val prefWidth = maxWidth + snappedLeftInset + snappedRightInset
          val viewPortWidth = scrollPane.getViewportBounds match {
            case null => 0
            case vp => vp.getWidth
          }
          computedPrefWidth = math.max(prefWidth, viewPortWidth)
        }
        computedPrefWidth
      }

      override protected def computePrefHeight (width :Double) = {
        if (computedPrefHeight < 0) {
          val linesHeight = lineNodes.getChildren.map(_.asInstanceOf[Text]).map(
            t => Utils.getLineHeight(t.getFont, t.getBoundsType)).sum
          val prefHeight = linesHeight + snappedTopInset + snappedBottomInset
          val viewPortHeight = scrollPane.getViewportBounds match {
            case null => 0
            case vp => vp.getHeight
          }
          computedPrefHeight = math.max(prefHeight, viewPortHeight)
        }
        computedPrefHeight
      }

      override protected def computeMinWidth (height :Double) = {
        if (computedMinWidth < 0) {
          val hInsets = snappedLeftInset + snappedRightInset
          computedMinWidth = Math.min(characterWidth + hInsets, computePrefWidth(height))
        }
        computedMinWidth
      }

      override protected def computeMinHeight (width :Double) = {
        if (computedMinHeight < 0) {
          val vInsets = snappedTopInset + snappedBottomInset
          computedMinHeight = Math.min(lineHeight + vInsets, computePrefHeight(width))
        }
        computedMinHeight
      }

      override def layoutChildren () {
        // position our lines
        val topPadding = snappedTopInset
        val leftPadding = snappedLeftInset
        (topPadding /: lineNodes.getChildren)((y, n) => {
          n.setLayoutX(leftPadding)
          n.setLayoutY(y)
          y + n.getBoundsInLocal.getHeight
        })

        // TODO: position caret, update selection-related nodes

        if (scrollPane.getPrefViewportWidth == 0 || scrollPane.getPrefViewportHeight == 0) {
          updatePrefViewportWidth()
          updatePrefViewportHeight()
          if (getParent != null && (
            scrollPane.getPrefViewportWidth > 0 || scrollPane.getPrefViewportHeight > 0)) {
            // Force layout of viewRect in ScrollPaneSkin
            getParent.requestLayout()
          }
        }
      }
    }
    private val contentNode = new ContentNode()
    contentNode.paddingProperty.addListener(new InvalidationListener() {
      override def invalidated (valueModel :Observable) {
        updatePrefViewportWidth()
        updatePrefViewportHeight()
      }
    })

    // put our scene graph together
    contentNode.getChildren.add(lineNodes)
    scrollPane.setContent(contentNode)
    getChildren.add(scrollPane)

    // TEMP: create our text nodes based on the contents of our buffer
    ctrl.buffer.lines.zipWithIndex foreach (addLineNode _).tupled

    // now that we've added our lines, we can update our font metrics and our preferred sizes
    updateFontMetrics()
    updatePrefViewportWidth()
    updatePrefViewportHeight()
    // if (textArea.isFocused()) setCaretAnimating(true);

    override def layoutChildren (x :Double, y :Double, w :Double, h :Double) {
      scrollPane.setHbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
      scrollPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
      super.layoutChildren(x, y, w, h)
      val bounds = scrollPane.getViewportBounds
      if (bounds != null && (bounds.getWidth < contentNode.minWidth(-1) ||
        bounds.getHeight < contentNode.minHeight(-1))) {
        scrollPane.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
        scrollPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
      }
    }

    protected def invalidateMetrics () {
      computedMinWidth = Double.NegativeInfinity
      computedMinHeight = Double.NegativeInfinity
      computedPrefWidth = Double.NegativeInfinity
      computedPrefHeight = Double.NegativeInfinity
    }

    private def addLineNode (line :Line, idx :Int) {
      val node = new Text(line.asString)
      node.setTextOrigin(VPos.TOP)
      node.setManaged(false)
      node.getStyleClass.add("text")
      lineNodes.getChildren.add(idx, node)
      // node.fontProperty.bind(ctrl.fontProperty)
      // node.fillProperty.bind(textFill)
      // node.impl_selectionFillProperty().bind(highlightTextFill)
    }

    private def getScrollTopMax :Double =
      math.max(0, contentNode.getHeight - scrollPane.getViewportBounds.getHeight)
    private def getScrollLeftMax :Double =
      math.max(0, contentNode.getWidth - scrollPane.getViewportBounds.getWidth)

    private def scrollBoundsToVisible (bounds :Rectangle2D) {
      val ctrl = getSkinnable
      val vpBounds = scrollPane.getViewportBounds
      val vpWidth = vpBounds.getWidth
      val vpHeight = vpBounds.getHeight
      val scrollTop = ctrl.scrollTop.get
      val scrollLeft = ctrl.scrollLeft.get
      val slop = 6.0

      if (bounds.getMinY < 0) {
        var y = scrollTop + bounds.getMinY
        if (y <= contentNode.snappedTopInset) y = 0
        ctrl.scrollTop.set(y)
      } else if (contentNode.snappedTopInset + bounds.getMaxY > vpHeight) {
        var y = scrollTop + contentNode.snappedTopInset + bounds.getMaxY - vpHeight
        if (y >= getScrollTopMax - contentNode.snappedBottomInset) y = getScrollTopMax
        ctrl.scrollTop.set(y)
      }

      if (bounds.getMinX < 0) {
        var x = scrollLeft + bounds.getMinX - slop
        if (x <= contentNode.snappedLeftInset + slop) x = 0
        ctrl.scrollLeft.set(x)
      } else if (contentNode.snappedLeftInset + bounds.getMaxX > vpWidth) {
        var x = scrollLeft + contentNode.snappedLeftInset + bounds.getMaxX - vpWidth + slop
        if (x >= getScrollLeftMax - contentNode.snappedRightInset - slop) x = getScrollLeftMax
        ctrl.scrollLeft.set(x)
      }
    }

    private def updatePrefViewportWidth () {
      val columnCount = 80 // TODO: getSkinnable.getPrefColumnCount
      scrollPane.setPrefViewportWidth(columnCount * characterWidth +
        contentNode.snappedLeftInset + contentNode.snappedRightInset)
    }
    private def updatePrefViewportHeight () {
      val rowCount = 24 // TODO: getSkinnable.getPrefRowCount
      scrollPane.setPrefViewportHeight(rowCount * lineHeight +
        contentNode.snappedTopInset + contentNode.snappedBottomInset)
    }
  }

  object StyleableProperties {
    val FONT = new FontCssMetaData[CodeArea]("-fx-font", Font.getDefault()) {
      override def isSettable (n :CodeArea) = (n.font == null) || !n.font.isBound
      override def getStyleableProperty (n :CodeArea) :StyleableProperty[Font] = n.font
    }

    val STYLEABLES :java.util.List[CssMetaData[_ <: Styleable, _]] = {
      val styleables = new java.util.ArrayList[CssMetaData[_ <: Styleable, _]](
        Control.getClassCssMetaData())
      styleables.add(FONT)
      java.util.Collections.unmodifiableList(styleables)
    }
  }

  def getClassCssMetaData :java.util.List[CssMetaData[_ <: Styleable, _]] =
    StyleableProperties.STYLEABLES
}
