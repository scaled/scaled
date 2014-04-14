//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import reactual.{Future, Signal, Value}

import scaled._

// TODO: should the point be automatically adjusted when text is inserted into the buffer before
// the point?

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[BufferArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (editor :Editor, _buffer :BufferImpl, initWid :Int, initHei :Int)
    extends RBufferView(initWid, initHei) {

  private val _lines = ArrayBuffer[LineViewImpl]() ++ _buffer.lines.map(new LineViewImpl(_))
  private val _changed = Signal[BufferView.Change]()
  override def changed = _changed

  def clearEphemeralPopup () {
    if (popup.isDefined && popup().isEphemeral) popup.clear()
  }

  // narrow the return types of these guys for our internal friends
  override def buffer :BufferImpl = _buffer
  override def lines :Seq[LineViewImpl] = _lines

  // when the buffer is edited: add, remove and update lines
  _buffer.edited.onValue { _ match {
    case Buffer.Insert(start, end, _) =>
      // the first line changed, the rest are new
      _lines(start.row).invalidate()
      if (end.row > start.row) {
        val row = start.row+1
        val added = _buffer.lines.slice(row, end.row+1)
        val newlns = added map(new LineViewImpl(_))
        _lines.insert(row, newlns :_*)
        _changed.emit(BufferView.Change(row, added.length, this))
      }

    case ed @ Buffer.Delete(start, deleted, _) =>
      // the first line changed, the rest are gone
      _lines(start.row).invalidate()
      val end = ed.end
      if (end.row > start.row) {
        val row = start.row+1 ; val deleted = end.row-row+1
        _lines.remove(row, deleted)
        _changed.emit(BufferView.Change(row, -deleted, this))
      }

    case ed @ Buffer.Transform(start, original, _) =>
      val end = ed.end
      start.row to end.row foreach { row => _lines(row).invalidate() }
  }}
  // pass style changes onto the line views
  _buffer.lineStyled.onValue { loc => _lines(loc.row).onStyle(loc) }
}
