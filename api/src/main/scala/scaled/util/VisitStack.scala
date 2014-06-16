//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scala.collection.mutable.ArrayBuffer
import scaled._

/** Maintains a stack of `(store, loc)` "visits". This useful when a mode provides some means to
  * navigate easily through the code and the user is likely to want to "pop back" to where they were
  * previously. Simply push the current location onto a visit stack before jumping to the new
  * location and invoke [[pop]] when you want to return to a previous location.
  *
  * @param name a name reported to the user if they attempt to pop the stack and it is empty.
  */
class VisitStack (name :String) {

  /** Pushes the buffer and point for `view` on the stack. */
  def push (view :BufferView) :Unit = push(view.buffer, view.point())

  /** Pushes `buffer.store` and `loc` onto the stack. */
  def push (buffer :BufferV, loc :Loc) :Unit = push(buffer.store, loc)

  /** Pushes `(store, loc)` onto the visit stack. */
  def push (store :Store, loc :Loc) {
    _stack += (store -> loc)
  }

  /** Pops the last `(store, loc)` from the stack and visits it. */
  def pop (editor :Editor) {
    if (_stack.isEmpty) editor.emitStatus(s"$name stack is empty.")
    else {
      val (store, loc) = _stack.last
      _stack.remove(_stack.size-1)
      editor.visitFile(store).point() = loc
    }
  }

  private val _stack = ArrayBuffer[(Store,Loc)]()
}
