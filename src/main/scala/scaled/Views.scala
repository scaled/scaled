//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Value, ValueV}

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

  /** That which handles undoing and redoing for this buffer. */
  def undoer :Undoer

  /** The current point (aka the cursor position). */
  def point :Loc

  /** Updates the current point. The point will be [[Buffer.bound]] into the buffer. */
  def point_= (loc :Loc) :Unit

  /** The width of the buffer, in characters. */
  def width :Int

  /** The height of the buffer, in characters. */
  def height :Int

  /** Prompts the user to input a string via the minibuffer. */
  def minibufferRead (prompt :String, defval :String) :Future[String]
  // TODO: minibufferRead variant that takes a tab-completer? mode provides?

  /** Emits a message to the status view associated with this buffer. */
  def emitStatus (msg :String)
}

/** A reactive version of [BufferView], used by modes. */
abstract class RBufferView extends BufferView {

  /** The (reactive) buffer being displayed by this view. */
  override def buffer :RBuffer

  /** The number of times the currently executing fn has been repeatedly executed. Normally this is
    * zero, but will be positive for each repeated execution of a fn. This is a minor hack to allow
    * fns to behave specially if they are invoked repeatedly by the user (with no intervening
    * actions). */
  def repeatedFn :Int

  /** The current point (aka the cursor position). */
  def pointV :ValueV[Loc]

  /** The width of the buffer view, in characters. */
  val widthV :Value[Int] = Value(80) // TODO: get values from config

  /** The height of the buffer view, in characters. */
  val heightV :Value[Int] = Value(24) // TODO: get values from config

  /** The index of the line at the top of the view. */
  val scrollTopV :Value[Int] = Value(0)

  /** The column index of the character at the left of the view. */
  val scrollLeftV :Value[Int] = Value(0)

  /** Adjusts the scroll position of this view by `delta` lines. The scroll position will be bounded
    * based on the size of the buffer. The point will then be bounded into the visible area of the
    * buffer. */
  def scrollVert (delta :Int) {
    val ctop = scrollTopV.get
    // bound bottom first, then top; this snaps buffers that are less than one screen tall to top
    // TODO: nix buffer.lines.length, use lines.length when lines is implemented
    val ntop = math.max(math.min(ctop + delta, buffer.lines.length - height), 0)
    // println(s"Updating scroll top ($delta ${lines.length} $height) $ctop => $ntop")
    scrollTopV.update(ntop)

    val p = point
    if (p.row < ntop) point = p.atRow(ntop)
    else if (p.row >= ntop + height) point = p.atRow(ntop + height - 1)
  }

  // implement some BufferView methods in terms of our reactive values
  override def point = pointV.get
  override def width = widthV.get
  override def height = heightV.get
}
