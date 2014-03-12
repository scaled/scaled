//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import reactual.{Future, Value}

import scaled._

// TODO: should the point be automatically adjusted when text is inserted into the buffer before
// the point?

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[BufferArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (editor :Editor, _buffer :BufferImpl, initWid :Int, initHei :Int)
    extends RBufferView(initWid, initHei) {

  private val _lines = ArrayBuffer[LineViewImpl]() ++ _buffer.lines.map(new LineViewImpl(_))

  private var _curFn :String = _
  private var _prevFn :String = _
  override def curFn = _curFn
  override def prevFn = _prevFn

  /** Called by [[KeyDispatcher]] just before it invokes a fn. */
  def willExecFn (fn :FnBinding) {
    _curFn = fn.name
    editor.clearStatus()
    undoStack.actionWillStart()
  }

  /** Called by [[KeyDispatcher]] just after it invokes a fn. */
  def didExecFn (fn :FnBinding) {
    undoStack.actionDidComplete()
    _prevFn = _curFn
    _curFn = null
  }

  /** Called by [[KeyDispatcher]] when the user presses an undefined key combination. */
  def didMissFn () {
    _prevFn = null
  }

  val undoStack = new UndoStack(this)
  override def undoer = undoStack

  private val _point = Value(Loc(0, 0))
  override def pointV = _point
  override def point_= (loc :Loc) = _point.update(_buffer.bound(loc))

  // narrow the return types of these guys for our internal friends
  override def buffer :BufferImpl = _buffer
  override def lines :Seq[LineViewImpl] = _lines

  // respond to buffer changes by adding/removing line views
  _buffer.edited.onValue { change =>
    if (change.deleted > 0) {
      // _lines.slice(change.offset, change.offset+change.deleted) foreach onDeleted
      _lines.remove(change.offset, change.deleted)
    }
    if (change.added > 0) {
      val added = _buffer.lines.slice(change.offset, change.offset+change.added)
      val newlns = added map(new LineViewImpl(_))
      _lines.insert(change.offset, newlns :_*)
    }
  }
}
