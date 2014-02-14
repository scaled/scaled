//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Value}

/** The visualization of a text buffer. It also manages the UX for manipulating and editing the
  * buffer. This includes:
  * - a series of [LineView] instances visualizing each line of text
  * - the point, which defines the cursor/insertion point and the point end of the point/mark
  * - the mark, the other end of the point/mark
  * - the scroll position of the view, which indicates which lines of the buffer are visible
  * Anything other than the data model for the buffer itself (which is encapsulated in [Buffer])
  * will be handled by this view.
  */
trait BufferView {

  /** The buffer being displayed by this view. */
  def buffer :Buffer

  /** Views for the lines in this buffer. */
  def lines :Seq[LineView]

  /** The current point (aka the cursor position). */
  def point :Loc

  /** Updates the current point. */
  def point_= (loc :Loc) :Unit

  /** The current mark, if any. */
  def mark :Option[Loc]

  /** Sets the current mark to `loc`. */
  def mark_= (loc :Loc) :Unit

  /** Clears the current mark. */
  def clearMark () :Unit

  /** Prompts the user to input a string via the minibuffer. */
  def minibufferRead (prompt :String, defval :String) :Future[String]

  /** Emits a message to the status view associated with this buffer. */
  def emitStatus (msg :String)
}

/** A reactive version of [BufferView], used by modes. */
trait RBufferView extends BufferView {

  /** The (reactive) buffer being displayed by this view. */
  override def buffer :RBuffer

  /** Reactive views for the lines in this buffer. */
  def rlines :Seq[RLineView]

  /** The current point (aka the cursor position) as a reactive value. */
  def pointV :Value[Loc]

  /** The current mark, if any, as a reactive value. */
  def markV :Value[Option[Loc]]
}

/** Visualizes a single line of text, potentially with style information. */
trait LineView {

  // TBD
}

/** A reactive version of [LineView], used by modes. */
trait RLineView {

  // TBD
}
