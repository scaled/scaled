//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.tk.Toolkit
import javafx.application.Platform
import javafx.beans.binding.{Bindings, ObjectBinding}
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
import javafx.scene.paint.{Color, Paint}
import javafx.scene.shape.Rectangle
import javafx.scene.text.{Font, Text, TextBoundsType}

import java.util.concurrent.Callable
import scaled._

/** Displays a buffer and passes events through to a dispatcher. */
class BufferArea (val bview :BufferViewImpl, val disp :DispatcherImpl) extends Region {

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
      if (!fontSetByCss) Dep.reapplyCSS(BufferArea.this)
    }
  }
  font.addListener(new InvalidationListener() {
    override def invalidated (valueModel :Observable) = updateFontMetrics()
  })

  /** Returns the number of characters that we can fit in the specified pixel width. */
  def widthInChars (width :Double) :Int = (width / charWidth).toInt
  /** Returns the number of characters that we can fit in the specified pixel height. */
  def heightInChars (height :Double) :Int = (height / lineHeight).toInt

  /** Returns this area's current width, in characters. */
  def width :Int = widthInChars(getWidth)
  /** Returns this area's current height, in lines. */
  def height :Int = heightInChars(getHeight)

  /** Tells this buffer area that it's going into the background. This frees up some resources that
    * are easy enough to recreate if/when the buffer area is made active again. */
  def hibernate () {
    // remove all of our lines, we'll add them back in on reactivation
    contentNode.removeLines(0, lineNodes.getChildren.size)
  }

  private var charWidth  = 0d
  private var lineHeight = 0d

  private def updateFontMetrics () {
    val fm = Toolkit.getToolkit.getFontLoader.getFontMetrics(font.get)
    charWidth = fm.computeStringWidth("W")
    // TODO: for some reason JavaFX always ends up one pixel taller when measuring text height; I
    // don't know where this magical pixel comes in, but until I can figure it out, I'm just
    // hacking in a pixel here; yay!
    lineHeight = math.ceil(fm.getLineHeight)+1
    // update the size of our cursors
    cursorBlock.setWidth(charWidth)
    cursorBlock.setHeight(lineHeight)
    uncursor.setWidth(charWidth)
    uncursor.setHeight(lineHeight)
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
    cursor.setVisible(focused)
    uncursor.setVisible(!focused)
  }

  // contains the Text nodes for each line
  private val lineNodes = new Group()
  lineNodes.setManaged(false)

  // create our focused cursor
  private val cursorBlock = new Rectangle()
  cursorBlock.getStyleClass.add("cursor")
  private val cursorText = new Text()
  cursorText.setTextOrigin(VPos.TOP)
  cursorText.setManaged(false)
  // bind the cursor text fill to the fill of the buffer area's background
  cursorText.fillProperty.bind(Bindings.createObjectBinding(new Callable[Paint]() {
    def call = {
      if (getBackground == null) Color.WHITE else {
        val fills = getBackground.getFills
        if (fills == null || fills.isEmpty) Color.WHITE else fills.get(0).getFill
      }
    }
  }, backgroundProperty))
  private val cursor = new Group()
  cursor.setManaged(false)
  cursor.getChildren.addAll(cursorBlock, cursorText)
  cursor.setVisible(false) // default cursor to invisible

  // create our unfocused cursor
  private val uncursor = new Rectangle(0, 0, Color.TRANSPARENT)
  uncursor.getStyleClass.add("uncursor")
  uncursor.setManaged(false)
  uncursor.setVisible(false)

  // TODO: handle deletion of lines that include the point? that will probably result in the point
  // being moved, so maybe we don't need to worry about it

  // wire up the display of popups
  class PopWin extends VBox() {
    setManaged(false)
    getStyleClass.add("popwin")
    setPadding(new Insets(3))

    private[this] var _ax = 0d
    private[this] var _ay = 0d
    private[this] var _pos :Popup.Pos = _

    def display (pop :Popup) {
      clear() // clear any previous bits

      if (pop.isError) getStyleClass.add("errpop")
      else getStyleClass.remove("errpop")

      val pbuffer = BufferImpl.scratch("*popup*")
      pbuffer.insert(pbuffer.start, pop.lines)
      val pview = new BufferViewImpl(pbuffer, pop.lines.map(_.length).max, pop.lines.size)
      getChildren.add(new BufferArea(pview, disp))

      val line = bview.lines(math.min(pop.pos.y, bview.lines.size-1))
      _ax = line.charX(pop.pos.x, charWidth)
      _ay = line.getLayoutY
      _pos = pop.pos
    }

    def clear () {
      // TODO: fade the popup out?
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
        if (pw != getWidth || ph != getHeight) Platform.runLater(new Runnable() {
          def run = resize(pw, ph)
        })
        else {
          super.layoutChildren()
          val lx = _pos.vx(_ax, pw, getPadding.getLeft, getPadding.getRight)
          val ly = _pos.vy(_ay, ph, lineHeight)

          // our view bounds are a little tricky; the width/height is obtained from the width and
          // height of the main view; the x/y offset are from content node because we move the
          // content node's origin around when the buffer scrolls, not the main view
          val vbounds = BufferArea.this.getLayoutBounds
          val vx = contentNode.getLayoutX ; val vy = contentNode.getLayoutY
          val vw = vbounds.getWidth ; val vh = vbounds.getHeight

          // adjust the position if the popup is pushed off the screen
          val bounds = getLayoutBounds
          val lr = lx + bounds.getWidth + vx ; val lb = ly + bounds.getHeight + vy
          val dx = if (lx < -vx) -vx-lx else if (lr > vw) math.max(vx-lx, vw-lr) else 0
          val dy = if (ly < -vy) -vy-ly else if (lb > vh) math.max(vy-ly, vh-lb) else 0

          setLayoutX(lx+dx)
          setLayoutY(ly+dy)
          setVisible(true)
        }
      }
    }
  }

  private val popup = new PopWin()
  private def checkPopup (force :Boolean)(popOpt :Option[Popup]) = popOpt match {
    case None      => popup.clear()
    // if our content node is not currently laid out, it will recheck the popup when it's done
    case Some(pop) => if (!contentNode.isNeedsLayout || force) popup.display(pop)
  }
  bview.popup onValue checkPopup(false)

  // contains our line nodes and other decorative nodes (cursor, selection, etc.)
  class ContentNode extends Region {
    private var vizTop = 0

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
    bview.scrollTop onChange { (ntop, otop) => updateVizLines() }
    bview.scrollLeft onValue { left => contentNode.setLayoutX(-left*charWidth) }

    // listen for addition and removal of lines
    bview.changed onValue { change =>
      // println(s"Lines @${change.row} ${change.delta}")
      val top = bview.scrollTop() ; val bot = top+bview.height()+1
      val tstart = math.max(top, change.row)
      if (change.delta < 0) {
        // if the change overlaps the visible area, remove the visible lines
        val tend = math.min(change.row-change.delta, bot)
        if (tend > tstart) removeLines(tstart-top, tend-top)
      } else if (change.delta > 0) {
        // if the change overlaps the visible area, add the visible lines
        val tend = math.min(change.row+change.delta, bot)
        if (tend > tstart) addLines(tstart-top, bview.lines.slice(tstart, tend))
      }
      // if the change happened before the vizTop, it must be adjusted
      if (change.row < vizTop) vizTop += change.delta
      updateVizLines()
    }

    // if the buffer view height changes, update our viz lines as well
    bview.height onValue { height => updateVizLines() }

    def updateCursor (point :Loc) {
      val line = bview.buffer.line(point)
      val lineCount = bview.buffer.lines.length

      // use the line to determine the layout coordinates of the cursor
      val linev = bview.lines(point.row)
      val cx = linev.charX(point.col, charWidth) ; val cy = linev.getLayoutY
      cursor.setLayoutX(cx) ; cursor.setLayoutY(cy)
      uncursor.setLayoutX(cx) ; uncursor.setLayoutY(cy)

      // set the cursor "text" to the character under the point (if any)
      val cchar = if (point.row >= lineCount) "" else {
        if (point.col >= line.length) ""
        else String.valueOf(line.charAt(point.col))
      }
      cursorText.setText(cchar)

      // if the cursor is out of view, scroll it back to the center of the screen
      val scrollTop = bview.scrollTop() ; val height = bview.height()
      // only force a recenter if we're beyond the hard top max (the point at which none of our
      // lines are visible on screen); this could happen if the buffer shrinks a bunch
      val hardTopMax = math.max(0, lineCount-1)
      if (point.row < scrollTop || point.row >= scrollTop + height || scrollTop > hardTopMax) {
        // if we do adjust due to hard top max overflow, adjust back to soft top max, which
        // means putting a whole screen full of lines in view, not just one
        val softTopMax = math.max(0, lineCount-height+1)
        bview.scrollTop() = math.min(softTopMax, math.max(0, point.row-height/2))
      }

      val scrollLeft = bview.scrollLeft() ; val width = bview.width()
      val scrollLeftMax = math.max(0, line.length-width+1)
      if (point.col < scrollLeft || point.col >= scrollLeft + width || scrollLeft > scrollLeftMax)
        bview.scrollLeft() = math.min(scrollLeftMax, math.max(0, point.col-width+1))

      // println(s"Cursor at ${point.col} x ${point.row} => " +
      //         s"${cursor.getLayoutX} x ${cursor.getLayoutY}")
    }

    // make this visible
    override def getChildren = super.getChildren

    override def layoutChildren () {
      // position our lines at the proper y offset
      val x = snappedLeftInset ; var y = snappedTopInset
      val iter = lineNodes.getChildren.iterator
      while (iter.hasNext) {
        val line = iter.next
        line.setLayoutX(x)
        line.setLayoutY(y)
        y += lineHeight
      }

      // position the whole contentNode at the proper x offset
      contentNode.setLayoutX(-bview.scrollLeft()*charWidth)

      // position the cursor
      updateCursor(bview.point())

      // if we have a popup, we might need to show it now that we know where everything is
      checkPopup(true)(bview.popup.getOption)
    }

    def updateVizLines () {
      val viewLines = bview.height()+1 ; val top = bview.scrollTop()
      val lines = bview.lines ; val bot = math.min(top + viewLines, lines.size)
      val preChildCount = lineNodes.getChildren.size
      val otop = vizTop

      try {
        // trim or add lines at the top, as needed
        if (otop < top) removeLines(0, top-otop)
        else if (otop > top) addLines(0, lines.slice(top, math.min(otop, bot)))

        val childCount = lineNodes.getChildren.size
        if (childCount > viewLines) removeLines(viewLines, childCount)
        else if (childCount < viewLines) addLines(childCount, lines.slice(top+childCount, bot))

        vizTop = top
        requestLayout()

      } catch {
        case e :Throwable =>
          throw new RuntimeException(
            s"updateVizLines otop=$otop top=$top bot=$bot lines=${lines.size} " +
              s"vlines=$viewLines ccount=$preChildCount", e)
      }
    }

    def addLines (index :Int, lines :SeqV[LineViewImpl]) {
      // println(s"addLines @$index +${lines.size}")
      lines.foreach { _.validate() }
      lineNodes.getChildren.addAll(index, lines.asJList)
    }
    def removeLines (from :Int, to :Int) {
      val capped = math.min(to, lineNodes.getChildren.size)
      // println(s"removeLines [$from, $to/$capped)")
      lineNodes.getChildren.subList(from, capped).foreach {
        _.asInstanceOf[LineViewImpl].invalidate() }
      lineNodes.getChildren.remove(from, capped)
    }
  }
  private val contentNode = new ContentNode()
  contentNode.setManaged(false)

  // put our scene graph together
  contentNode.getChildren.addAll(lineNodes, cursor, uncursor, popup)
  getChildren.add(contentNode)

  // move the cursor when the point is updated
  bview.point onValueNotify contentNode.updateCursor

  // display the lines visible at the top of the buffer
  contentNode.updateVizLines()

  // request relayout when our view width/height changes
  bview.width onValue { _ => requestLayout() }
  bview.height onValue { _ => requestLayout() }

  // TODO: insets?
  override protected def computeMinWidth (height :Double) = 20 * charWidth // ?
  override protected def computeMinHeight (height :Double) = lineHeight
  override protected def computePrefWidth (height :Double) = bview.width() * charWidth
  override protected def computePrefHeight (width :Double) = bview.height() * lineHeight
  override protected def computeMaxWidth (height :Double) = Double.MaxValue
  override protected def computeMaxHeight (width :Double) = Double.MaxValue

  override def layoutChildren () {
    contentNode.layoutChildren()
  }

  override def resize (nw :Double, nh :Double) {
    super.resize(nw, nh)

    // resize seems to be called any time anything happens, so avoid doing a lot of extra work if
    // our size didn't actually change
    val nwc = widthInChars(nw) ; val nhc = heightInChars(nh)
    if (nwc != bview.width() || nhc != bview.height()) {
      // println(s"VP resized $nw x $nh -> $nwc x $nhc")
      wasResized(nwc, nhc)
      // TODO: update scrollTop/scrollLeft if needed?
    }
  }

  protected def wasResized (widthChars :Int, heightChars :Int) {
    // update our buffer view size with our resized (char) width/height; NOTE: this loops back
    // around causing this BufferArea to "prefer" this width and height if asked; we may want to
    // break that loop and separately handle current versus preferred size in BufferView
    bview.width() = widthChars
    bview.height() = heightChars
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
  import java.util.{List => JList, ArrayList, Collections}

  object StyleableProperties {
    val FONT = new FontCssMetaData[BufferArea]("-fx-font", Font.getDefault()) {
      override def isSettable (n :BufferArea) = (n.font == null) || !n.font.isBound
      override def getStyleableProperty (n :BufferArea) :StyleableProperty[Font] = n.font
    }

    val STYLEABLES :JList[CssMetaData[_ <: Styleable, _]] = {
      val styleables = new ArrayList[CssMetaData[_ <: Styleable, _]](
        Region.getClassCssMetaData())
      styleables.add(FONT)
      Collections.unmodifiableList(styleables)
    }
  }

  def getClassCssMetaData :JList[CssMetaData[_ <: Styleable, _]] =
    StyleableProperties.STYLEABLES

  // HACK: terrible hack to work around JavaFX bug or my misunderstanding + misuse
  def resetStyleHelper (area :BufferArea) :Unit = StyleHelperField.set(area, null)
  private val StyleHelperField = find(classOf[BufferArea], "styleHelper")
  private def find (cl :Class[_], nm :String) :java.lang.reflect.Field = {
    for (field <- cl.getDeclaredFields) {
      if (field.getName == nm) {
        field.setAccessible(true)
        return field
      }
    }
    find(cl.getSuperclass, nm)
  }
}
