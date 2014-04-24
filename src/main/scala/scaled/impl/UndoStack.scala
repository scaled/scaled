//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import scaled._

/** Tracks changes to a buffer, aggregating sequences of individual changes into single undoable
  * actions. Then handles reversing said actions, on request.
  */
class UndoStack (buffer :BufferImpl) extends Undoer {

  buffer.edited.onValue { edit => accum += edit }

  def actionWillStart (point :Loc) {
    _point = point
    // if the buffer is currently clean, move the undo clean pointer here
    if (!buffer.dirty) {
      _cleanUndoIdx = _actions.size
      _cleanRedoIdx = -1 // and wipe the clean redo index
    }
  }

  def actionDidComplete () :Unit = if (!_edits.isEmpty) {
    accumTo(_edits, _actions)
    // since we've applied one or more normal edits, clear the redo buffer
    _redoActions.clear()
    _cleanRedoIdx = -1
  }

  override def undo () :Option[Loc] = {
    if (_actions.isEmpty) None
    else {
      // if the buffer is clean prior this undo, move the redo clean pointer here
      if (!buffer.dirty) _cleanRedoIdx = _redoActions.size
      // pop the most recent action from the undo stack and undo it
      val action = pop(_actions)
      _undoing = true
      try action.undo()
      finally _undoing = false
      // accumulate the undone edits to the redo stack as a redo action
      accumTo(_redoEdits, _redoActions)
      // if we just undid to the last clean pointer, mark the buffer clean
      if (_cleanUndoIdx == _actions.size) buffer.dirtyV() = false
      // finally return the point associated with this undone action
      Some(action.point)
    }
  }

  override def redo () :Option[Loc] = {
    if (_redoActions.isEmpty) None
    else {
      // pop the most recent action from the redo stack and undo it (undoing and undo)
      val action = pop(_redoActions)
      action.undo()
      // accumulate the redone edits immediately so that the actionDidComplete (that naturally
      // follows a redo) does not see uncommitted edits and think that the user just made a normal
      // edit (which would clear the redo list)
      accumTo(_edits, _actions)
      // if we just redid to the last clean pointer, mark the buffer clean
      if (_cleanRedoIdx == _redoActions.size) buffer.dirtyV() = false
      // finally return the point associated with this redone action
      Some(action.point)
    }
  }

  private def pop (actions :ArrayBuffer[Action]) = {
    val action = actions.last ; actions.trimEnd(1) ; action
  }

  // returns the buffer onto which to accumulate buffer edits; normally we accumulate to the edits
  // buffer, but when we're undoing, we accumulate edits to the redo buffer because the edits that
  // come in during that time are triggered by our undoing, not by user actions
  private def accum = if (_undoing) _redoEdits else _edits

  private def accumTo (edits :ArrayBuffer[Undoable], actions :ArrayBuffer[Action]) {
    if (!edits.isEmpty) {
      // determine whether these edits are "typing" (inserting or deleting a single char), and
      // whether we can accumulate them with the last action on the stack
      val (isTyping, canAccum) = edits match {
        case Seq(le :Buffer.Edit) =>
          val isSingleChar = le.end == le.start.nextC
          val isBreak = isBreakChar(buffer.charAt(le.start))
          (isSingleChar, isSingleChar && !isBreak && actions.size > 0 && actions.last.canAccum(le))
        case _ => (false, false)
      }

      val action = if (canAccum) {
        val accum = actions.last.accum(edits)
        actions.trimEnd(1)
        accum
      }
      else Action(_point, isTyping, Seq() ++ edits)
      actions += action
      edits.clear()
    }
  }

  private def isBreakChar (c :Char) = Character.isWhitespace(c) // TODO: delegate this to the mode

  private val _edits = ArrayBuffer[Undoable]()
  private val _actions = ArrayBuffer[Action]()
  private val _redoEdits = ArrayBuffer[Undoable]()
  private val _redoActions = ArrayBuffer[Action]()
  private var _point = Loc(0, 0)
  private var _undoing = false
  private var _redoing = false
  private var _cleanUndoIdx = 0
  private var _cleanRedoIdx = -1

  private case class Action (point :Loc, isTyping :Boolean, edits :Seq[Undoable]) {
    // undo the edits in the reverse of the order they were accumulated
    def undo () = edits.reverse.foreach { _.undo() }
    // accumulates additional edits to this action
    def accum (edits :Seq[Undoable]) = Action(point, isTyping, this.edits ++ edits)
    // we must be a typing edit, and the new edit must be the same kind (insert vs. delete)
    def canAccum (edit :Buffer.Edit) = isTyping && edits.head.getClass == edit.getClass
  }
}
