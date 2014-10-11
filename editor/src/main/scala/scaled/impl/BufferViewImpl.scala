//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import scaled._

// TODO: should the point be automatically adjusted when text is inserted into the buffer before
// the point?

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[BufferArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (editor :Editor, _buffer :BufferImpl, initWid :Int, initHei :Int)
    extends RBufferView(initWid, initHei) {

  private val _lines = new SeqBuffer[LineViewImpl](_buffer.lines.size)
  _buffer.lines foreach { _lines += new LineViewImpl(_) }

  private val _changed = Signal[BufferView.Change]()
  override def changed = _changed

  def clearEphemeralPopup () {
    if (popup.isDefined && popup().isEphemeral) popup.clear()
  }

  // narrow the return types of these guys for our internal friends
  override def buffer :BufferImpl = _buffer
  override def lines :SeqV[LineViewImpl] = _lines

  // when the buffer is edited: add, remove and update lines
  _buffer.edited.onValue { _ match {
    case Buffer.Insert(start, end) =>
      // the first line changed, the rest are new
      _lines(start.row).invalidate()
      if (end.row > start.row) {
        val row = start.row+1
        val added = _buffer.lines.slice(row, end.row+1)
        _lines.insert(row, added map(new LineViewImpl(_)))
        _changed.emit(BufferView.Change(row, added.length, this))
      }
      // now update the point based on the insert
      point() = Loc.adjustForInsert(point(), start, end)

    case Buffer.Delete(start, end, deleted) =>
      // update the point based on the delete before deleting the lines
      point() = Loc.adjustForDelete(point(), start, end)
      // the first line changed, the rest are gone
      _lines(start.row).invalidate()
      if (end.row > start.row) {
        val row = start.row+1 ; val deleted = end.row-row+1
        _lines.remove(row, deleted)
        _changed.emit(BufferView.Change(row, -deleted, this))
      }

    case Buffer.Transform(start, end, _) =>
      start.row to end.row foreach { row => _lines(row).invalidate() }
  }}
  // pass style changes onto the line views
  _buffer.lineStyled.onValue { loc => _lines(loc.row).onStyle(loc) }
}
