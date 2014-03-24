//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, OptValue, Value, ValueV}

/** Visualizes a single line of text, potentially with style information. */
abstract class LineView {

  /** The line being displayed by this view. */
  def line :LineV

  // TODO: style runs, margin decorations
  // TOOD: access to the JavaFX scene graph Node on which to anchor bits?
}

/** The visualization of a text buffer. It also manages the UX for manipulating and editing the
  * buffer. This includes:
  * - a series of [LineView] instances visualizing each line of text
  * - the point, which defines the cursor/insertion point and the point end of the point/mark
  * - the mark, the other end of the point/mark
  * - the scroll position of the view, which indicates which lines of the buffer are visible
  * Anything other than the data model for the buffer itself (which is encapsulated in [Buffer])
  * will be handled by this view.
  */
abstract class BufferView {

  /** The buffer being displayed by this view. */
  def buffer :Buffer

  /** Views for the lines in this buffer. */
  def lines :Seq[LineView]

  /** The current point (aka the cursor position). */
  def point :Loc

  /** Updates the current point. The point will be [[Buffer.bound]] into the buffer. */
  def point_= (loc :Loc) :Unit

  /** The width of the view, in characters. */
  def width :Int

  /** Sets the width of the view, in characters. This may be overridden with a smaller value if the
    * requested width cannot be accommodated. */
  def width_= (w :Int) :Unit

  /** The height of the view, in characters. */
  def height :Int

  /** Sets the height of the view, in characters. This may be overridden with a smaller value if the
    * requested height cannot be accommodated. */
  def height_= (w :Int) :Unit

  /** The index of the line at the top of the view. */
  def scrollTop :Int

  /** Sets the index of the lne at the top of the view. */
  def scrollTop_= (top :Int) :Unit

  /** The column index of the character at the left of the view. */
  def scrollLeft :Int

  /** Sets the column index of the character at the left of the view. */
  def scrollLeft_= (left :Int) :Unit

  /** Adjusts the scroll position of this view by `delta` lines. The scroll position will be bounded
    * based on the size of the buffer. The point will then be bounded into the visible area of the
    * buffer. */
  def scrollVert (delta :Int) {
    val ctop = scrollTop
    // bound bottom first, then top; this snaps buffers that are less than one screen tall to top
    // TODO: nix buffer.lines.length, use lines.length when lines is implemented
    val ntop = math.max(math.min(ctop + delta, buffer.lines.length - height), 0)
    // println(s"Updating scroll top ($delta ${lines.length} $height) $ctop => $ntop")
    scrollTop = ntop

    val p = point
    if (p.row < ntop) point = p.atRow(ntop)
    else if (p.row >= ntop + height) point = p.atRow(ntop + height - 1)
  }
}

/** A reactive version of [BufferView], used by modes. */
abstract class RBufferView (initWidth :Int, initHeight :Int) extends BufferView {

  /** The (reactive) buffer being displayed by this view. */
  override def buffer :RBuffer

  /** The current point (aka the cursor position). */
  def pointV :ValueV[Loc]

  /** The width of the buffer view, in characters. */
  val widthV :Value[Int] = Value(initWidth)

  /** The height of the buffer view, in characters. */
  val heightV :Value[Int] = Value(initHeight)

  /** The index of the line at the top of the view. */
  val scrollTopV :Value[Int] = Value(0)

  /** The column index of the character at the left of the view. */
  val scrollLeftV :Value[Int] = Value(0)

  /** The popup being displayed by this buffer, if any. */
  val popup :OptValue[Popup] = OptValue()

  // implement some BufferView methods in terms of our reactive values
  override def point = pointV()
  override def width = widthV()
  override def height = heightV()
  override def scrollTop = scrollTopV()
  override def scrollLeft = scrollLeftV()
  override def width_= (w :Int) = widthV() = w
  override def height_= (h :Int) = heightV() = h
  override def scrollTop_= (top :Int) = scrollTopV() = top
  override def scrollLeft_= (left :Int) = scrollLeftV() = left
}
