//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import scaled._
import scaled.util.Close

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[BufferArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (val window :WindowImpl, _buffer :BufferImpl, initWidth :Int, initHeight :Int)
    extends RBufferView(initWidth, initHeight) {

  private val _lines = new SeqBuffer[LineViewImpl](_buffer.lines.size)
  _buffer.lines foreach { _lines += new LineViewImpl(_) }

  private val _changed = Signal[BufferView.Change]()
  override def changed = _changed

  private val _toClose = Close.bag()

  val popupAdded = Signal[OptValue[Popup]]()
  override def addPopup (popup :Popup) = {
    val popopt = OptValue(popup)
    popupAdded.emit(popopt)
    popopt
  }

  def clearEphemeralPopup () {
    if (popup.isDefined && popup().isEphemeral) popup.clear()
  }

  /** Disconnects this view from its underlying buffer. */
  def dispose () {
    _toClose.close()
    // write our view state back to the buffer
    _buffer.viewState = BufferImpl.ViewState(point(), scrollTop(), scrollLeft())
  }

  // narrow the return types of these guys for our internal friends
  override def buffer :BufferImpl = _buffer
  override def lines :SeqV[LineViewImpl] = _lines

  // configure this view based on the buffer's latest view state
  { val vs = _buffer.viewState
    point() = vs.point
    scrollTop() = vs.scrollTop
    scrollLeft() = vs.scrollLeft }

  // when the buffer is edited: add, remove and update lines
  _toClose += _buffer.edited.onValue { _ match {
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
  _toClose += _buffer.lineStyled.onValue { loc => {
    if (loc.row >= _lines.length) {
      println(s"Bogus style notification $loc ([0..${_lines.length}))")
      Thread.dumpStack()
    }
    else _lines(loc.row).onStyle(loc)
  }}

  // force a refresh of the point whenever a buffer edit "intersects" the point
  // (TODO: this seems error prone, is there a better way?)
  _toClose += _buffer.edited.onValue { edit =>
    // the point may be temporarily invalid while edits are being undone, so NOOP in that case
    // because the correct point will be restored after the undo is completed
    val cp = point()
    val pointValid = cp.row < _lines.size
    if (pointValid && edit.contains(cp)) point.updateForce(cp)
  }
}
