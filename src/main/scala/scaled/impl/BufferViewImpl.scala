//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import reactual.{Future, Value}

import scaled._

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[CodeArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (_buffer :BufferImpl) extends RBufferView {

  private val _lines = ArrayBuffer[LineViewImpl]() ++ _buffer.lines.map(new LineViewImpl(_))

  val undoStack = new UndoStack(this)
  override def undoer = undoStack

  private val _point = Value(Loc(0, 0))
  override def pointV = _point
  override def point_= (loc :Loc) = _point.update(_buffer.bound(loc))

  // narrow the return types of these guys for our internal friends
  override def buffer :BufferImpl = _buffer
  override def lines :Seq[LineViewImpl] = _lines

  override def minibufferRead (prompt :String, defval :String) =
    Future.failure(new Exception("TODO"))
  // TODO: minibufferRead variant that takes a tab-completer? mode provides?

  override def emitStatus (msg :String) = println(s"STATUS: $msg")

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
