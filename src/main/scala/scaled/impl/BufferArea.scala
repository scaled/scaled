//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.application.Platform
import javafx.beans.binding.ObjectBinding
import javafx.beans.property.{DoubleProperty, ObjectProperty, SimpleDoubleProperty}
import javafx.beans.value.{ChangeListener, ObservableObjectValue, ObservableValue}
import javafx.beans.{InvalidationListener, Observable}
import javafx.css.{CssMetaData, FontCssMetaData}
import javafx.css.{Styleable, StyleableProperty, StyleOrigin, StyleableObjectProperty}
import javafx.event.EventHandler
import javafx.geometry.{Bounds, Insets, Rectangle2D, VPos}
import javafx.scene.Group
import javafx.scene.control.{Control, Label, SkinBase}
import javafx.scene.input.{MouseEvent, KeyCode, KeyEvent}
import javafx.scene.layout.{Region, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.scene.text.{Font, Text, TextBoundsType}

import com.sun.javafx.tk.Toolkit

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import scaled._

// TODO
//
// - support passing in start and end anchors and only displaying the part of the buffer between
// those anchors (actually make this an attribute of the BufferView)
//
// - how will we support styles? attribute the buffer? probably so because that will cause all
// views of the buffer to remain in sync as attributes are provided by external intelligence

/** Brings everything together into one all singing, all dancing text editing extravaganza.
  */
class BufferArea (editor :Editor, bview :BufferViewImpl, disp :DispatcherImpl)
    extends Region {

  getStyleClass.add("buffer")

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
        fontSetByCss = false
      }
    }

    override def set (value :Font) {
      if (value != get()) super.set(value)
    }

    override def getCssMetaData = BufferArea.StyleableProperties.FONT
    override def getBean = BufferArea.this
    override def getName = "font"

    override protected def invalidated () {
      // if font is changed by calling setFont, then css might need to be reapplied since font size
      // affects calculated values for styles with relative values
      if (!fontSetByCss) impl_reapplyCSS()
    }
  }
  font.addListener(new InvalidationListener() {
    override def invalidated (valueModel :Observable) = updateFontMetrics()
  })

  private var charWidth  = 0d
  private var lineHeight = 0d

  private def updateFontMetrics () {
    val fm = Toolkit.getToolkit.getFontLoader.getFontMetrics(font.get)
    charWidth = fm.computeStringWidth("W")
    // val firstLine = lineNodes.getChildren.get(0).asInstanceOf[Text]
    // lineHeight = Utils.getLineHeight(font.get, firstLine.getBoundsType)
    // lineHeight = Utils.getLineHeight(font.get, TextBoundsType.LOGICAL)

    // TODO: for some reason JavaFX always ends up one pixel taller when measuring text height; I
    // don't know where this magical pixel comes in, but until I can figure it out, I'm just
    // hacking in a pixel here; yay!
    lineHeight = math.ceil(fm.getLineHeight)+1
    // update the size of our cursor
    cursorBlock.setWidth(charWidth)
    cursorBlock.setHeight(lineHeight)
  }

  // forward key events to the control for dispatching
  private[this] val keyEventListener = new EventHandler[KeyEvent]() {
    override def handle (e :KeyEvent) = if (!e.isConsumed) disp.keyPressed(e)
  }
  addEventHandler(KeyEvent.ANY, keyEventListener)

  // listen for changes in focus
  focusTraversableProperty().setValue(true)
  focusedProperty.addListener(onChangeB(onFocusChange))
  private def onFocusChange (focused :Boolean) {
    cursor.setVisible(focused) // TODO: change to an outline around the char; except the
                               // minibuffer which should make the cursor actually invisible
  }

  // this tracks the maximum line length in the buffer
  private val maxLenTracker = new Utils.MaxLengthTracker(bview.buffer)

  // contains the Text nodes for each line
  private val lineNodes = new Group()
  lineNodes.setManaged(false)

  // create our cursor and bind its position to `bview.point`
  private val cursorBlock = new Rectangle()
  cursorBlock.getStyleClass.add("cursorBlock")
  private val cursorText = new Text()
  cursorText.setTextOrigin(VPos.TOP)
  cursorText.setManaged(false)
  cursorText.getStyleClass.add("cursorText")
  private val cursor = new Group()
  cursor.setManaged(false)
  cursor.getChildren.addAll(cursorBlock, cursorText)
  cursor.setVisible(false) // default cursor to invisible
  // move the cursor when the point is updated
  bview.pointV onValue contentNode.updateCursor
  // react to line edits by updating our views
  bview.buffer.lineEdited.onValue { change =>
    // println(s"Chars @${change.loc} +${change.added} -${change.deleted}")

    // update the visualization of the line that was edited
    bview.lines(change.loc.row).onEdit(change)

    // refresh the character shown on the cursor whenever a buffer edit "intersects" the point
    // (TODO: this seems error prone, is there a better way?)

    // the point may be temporarily invalid while edits are being undone, so NOOP in that case
    // because the correct point will be restored after the undo is completed
    val pointValid = bview.point.row < bview.buffer.lines.size
    // TODO: make this more precise?
    if (pointValid && bview.point.row == change.loc.row && change.deleted > 0) {
      contentNode.updateCursor(bview.point)
    }
  }
  bview.buffer.lineStyled.onValue { loc =>
    bview.lines(loc.row).onStyle(loc)
  }

  // TODO: handle deletion of lines that include the point? that will probably result in the point
  // being moved, so maybe we don't need to worry about it

  // wire up the display of popups
  class PopWin extends VBox() {
    setManaged(false)
    getStyleClass.add("popup")
    setPadding(new Insets(3))

    private[this] var _ax = 0d
    private[this] var _ay = 0d
    private[this] var _pos :Popup.Pos = _

    def display (pop :Popup) {
      clear() // clear any previous bits

      if (pop.isError) getStyleClass.add("errpop")
      else getStyleClass.remove("errpop")

      pop.text.foreach { line =>
        val text = new Text(line)
        text.setTextOrigin(VPos.TOP)
        getChildren.add(text)
      }

      val line = bview.lines(pop.pos.y)
      _ax = line.charX(pop.pos.x, charWidth)
      _ay = line.node.getLayoutY
      _pos = pop.pos
    }

    def clear () {
      popup.getChildren.clear()
      popup.setVisible(false)
      _pos = null
    }

    // we have to do some special jiggery pokery here because we can't just size and show ourselves
    // in display() because (for whatever stupid reason) our CSS font is not properly configured at
    // that point, so we have to wait for a deferred layout to come in after things are configured
    // and then if we're not at our preferred size, size, position and show ourselves
    override def layoutChildren () {
      if (_pos != null) {
        val pw = prefWidth(-1) ; val ph = prefHeight(-1)
        // we can't just call resize() directly here because JavaFX doesn't take kindly to a Node
        // resizing itself during the layout process, so we defer it
        if (pw != getWidth || ph != getHeight) defer { resize(pw, ph) }
        else {
          super.layoutChildren()
          setLayoutX(_pos.vx(_ax, pw, getPadding.getLeft, getPadding.getRight))
          setLayoutY(_pos.vy(_ay, ph, lineHeight))
          setVisible(true)
        }
      }
    }
  }
  private val popup = new PopWin()
  bview.popup.onValueNotify { _ match {
    case None      => popup.clear()
    case Some(pop) => popup.display(pop)
  }}

  // contains our line nodes and other decorative nodes (cursor, selection, etc.)
  class ContentNode extends Region {
    getStyleClass.add("content")

    // forward mouse events to the control
    addEventHandler(MouseEvent.MOUSE_PRESSED, new EventHandler[MouseEvent]() {
      override def handle (event :MouseEvent) {
        mousePressed(event)
        event.consume()
      }
    })
    addEventHandler(MouseEvent.MOUSE_RELEASED, new EventHandler[MouseEvent]() {
      override def handle (event :MouseEvent) {
        mouseReleased(event)
        event.consume()
      }
    })
    addEventHandler(MouseEvent.MOUSE_DRAGGED, new EventHandler[MouseEvent]() {
      override def handle (event :MouseEvent) {
        mouseDragged(event)
        event.consume()
      }
    })
    // move our lines when scrollTop/Left change
    bview.scrollTopV.onValue { top =>
      contentNode.setLayoutY(-bview.scrollTopV()*lineHeight) // TODO: put this in updateVizLines?
      updateVizLines()
    }
    bview.scrollLeftV.onValue { left =>
      // val range = maxLenTracker.maxLength - bview.width + 1
      // println(s"Scrolling left to $left ($range)")
      // scrollPane.setHvalue(math.min(1, left / range.toDouble))

      // TODO
    }

    def updateCursor (point :Loc) {
      // use the line to determine the layout coordinates of the cursor
      val line = bview.lines(point.row)
      cursor.setLayoutX(line.charX(point.col, charWidth))
      cursor.setLayoutY(line.node.getLayoutY)

      // set the cursor "text" to the character under the point (if any)
      val cchar = if (point.row >= bview.buffer.lines.length) "" else {
        val line = bview.buffer.line(point)
        if (point.col >= line.length) ""
        else String.valueOf(line.charAt(point.col))
      }
      cursorText.setText(cchar)

      // if the cursor is out of view, scroll it back to the center of the screen
      val (scrollTop, height) = (bview.scrollTopV(), bview.height)
      val scrollMax = bview.buffer.lines.length - bview.height + 1
      if (point.row < scrollTop || point.row >= scrollTop + height)
        bview.scrollTopV() = math.min(scrollMax, math.max(0, point.row - height/2))
      // TODO: same for horizontal scrolling?

      // println(s"Cursor at ${point.col} x ${point.row} => " +
      //         s"${cursor.getLayoutX} x ${cursor.getLayoutY}")
    }

    // make this visible
    override def getChildren = super.getChildren

    override def layoutChildren () {
      // position our lines at the proper y offset
      val leftPadding = snappedLeftInset
      val cs = lineNodes.getChildren
      @tailrec def loop (y :Double, idx :Int) {
        if (idx < cs.size) {
          val line = cs.get(idx)
          line.setLayoutX(leftPadding)
          line.setLayoutY(y)
          loop(y + lineHeight, idx+1)
        }
      }
      loop(snappedTopInset, 0)

      // update which lines are visible
      updateVizLines()

      // position the cursor
      updateCursor(bview.point)

      // TODO: update selection-related nodes
    }

    def updateVizLines () {
      val (top, left) = (bview.scrollTopV(), bview.scrollLeftV())
      val (bot, right) = (top+bview.height, left+bview.width)
      // println(s"Updating viz lines top=$top left=$left bot=$bot right=$right")

      // position the visible lines
      val cs = lineNodes.getChildren
      @tailrec def loop (idx :Int) {
        if (idx < cs.size) {
          cs.get(idx).setVisible(idx >= top && idx <= bot)
          loop(idx+1)
        }
      }
      loop(0)
    }
  }
  private val contentNode = new ContentNode()
  contentNode.setManaged(false)

  // put our scene graph together
  contentNode.getChildren.addAll(lineNodes, cursor, popup)
  getChildren.add(contentNode)

  // listen for addition and removal of lines
  bview.buffer.edited.onValue { change =>
    // println(s"Lines @${change.offset} +${change.added} -${change.deleted}")
    if (change.deleted > 0) {
      lineNodes.getChildren.remove(change.offset, change.offset+change.deleted)
      // TODO: let these nodes know they've been removed so they can cleanup?
    }
    if (change.added > 0) {
      val nodes = bview.lines.slice(change.offset, change.offset+change.added).map(_.node)
      lineNodes.getChildren.addAll(change.offset, nodes)
    }
    requestLayout()
  }
  // add all the current lines to the buffer
  lineNodes.getChildren.addAll(bview.lines.map(_.node) :_*)

  // now that we've added our lines, we can update our font metrics and our preferred sizes
  updateFontMetrics()
  // if (textArea.isFocused()) setCaretAnimating(true)

  // TODO: insets?
  override protected def computeMinWidth (height :Double) = 20 * charWidth // ?
  override protected def computeMinHeight (height :Double) = lineHeight
  override protected def computePrefWidth (height :Double) = bview.width * charWidth
  override protected def computePrefHeight (width :Double) = bview.height * lineHeight
  override protected def computeMaxWidth (height :Double) = Double.MaxValue
  override protected def computeMaxHeight (width :Double) = Double.MaxValue

  override def layoutChildren () {
    contentNode.layoutChildren()
  }

  override def resize (nw :Double, nh :Double) {
    super.resize(nw, nh)

    // resize seems to be called any time anything happens, so avoid doing a lot of extra work if
    // our size didn't actually change
    val nwc = (nw / charWidth).toInt
    val nhc = (nh / lineHeight).toInt
    if (nwc != bview.width || nhc != bview.height) {
      // update the character width/height in our buffer view
      bview.widthV() = nwc
      bview.heightV() = nhc
      // println(s"VP resized $nw x $nh -> ${bview.width} x ${bview.height}")

      // TODO: update scrollTop/scrollLeft if needed?

      // update visible lines
      contentNode.updateVizLines()

      // TODO: set clip rect?
    }
  }

  override def getCssMetaData = BufferArea.getClassCssMetaData

  // mouse events are forwarded here by the skin
  def mousePressed (mev :MouseEvent) {
    // TODO: update view.point to the clicked Loc
    // TODO: also note the clicked Loc so that if we drag, we can use it to set the region
  }
  def mouseDragged (mev :MouseEvent) {
    // TODO: adjust the point and mark to set the active region to the dragged area
  }
  def mouseReleased (mev :MouseEvent) {}
}

/** [BufferArea] helper classes and whatnot. */
object BufferArea {

  object StyleableProperties {
    val FONT = new FontCssMetaData[BufferArea]("-fx-font", Font.getDefault()) {
      override def isSettable (n :BufferArea) = (n.font == null) || !n.font.isBound
      override def getStyleableProperty (n :BufferArea) :StyleableProperty[Font] = n.font
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
