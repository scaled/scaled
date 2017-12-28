//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import scaled._

/** Tracks changes to a buffer, aggregating sequences of individual changes into single undoable
  * actions. Then handles reversing said actions, on request.
  */
class UndoStack (buffer :BufferImpl) extends Undoer {
  import UndoStack._

  // don't accumulate undo information for uneditable buffers
  buffer.edited.onValue { edit =>
    if (buffer.editable) accum += edit
  }

  def delimitAction (point :Loc) {
    // first commit any edits that came in since our last delimiting
    if (!_edits.isEmpty) {
      accumTo(_edits, _actions)
      // since we've applied one or more normal edits, clear the redo buffer
      _redoActions.clear()
      _cleanRedoIdx = -1
    }

    // now prepare to capture new edits up to our next delimiting
    _point = point
    // if the buffer is currently clean, move the undo clean pointer here
    if (!buffer.dirty) {
      _cleanUndoIdx = _actions.size
      _cleanRedoIdx = -1 // and wipe the clean redo index
    }
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
      catch {
        case t :Throwable => println(s"Undo choke: $action") ; t.printStackTrace(System.err)
      }
      finally _undoing = false
      // accumulate the undone edits to the redo stack as a redo action
      accumTo(_redoEdits, _redoActions)
      // if we just undid to the last clean pointer, mark the buffer clean
      if (_cleanUndoIdx == _actions.size) buffer.dirtyV() = false
      // reset our action point to the undone point
      _point = action.point
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

  override def accumNextEdit () {
    _accumNextEdit = true
  }

  private def pop (actions :SeqBuffer[Action]) = {
    val action = actions.last ; actions.trimEnd(1) ; action
  }

  // returns the buffer onto which to accumulate buffer edits; normally we accumulate to the edits
  // buffer, but when we're undoing, we accumulate edits to the redo buffer because the edits that
  // come in during that time are triggered by our undoing, not by user actions
  private def accum = if (_undoing) _redoEdits else _edits

  private def accumTo (edits :SeqBuffer[Buffer.Edit], actions :SeqBuffer[Action]) {
    if (!edits.isEmpty) {
      // if we can't accumulate these edits with the most recent action, add a new action
      if (!_accumNextEdit || actions.isEmpty || !actions.last.accum(edits)) {
        actions += toAction(_point, edits)
      }
      edits.clear()
      _accumNextEdit = false
    }
  }

  private def isBreakChar (c :Char) = Character.isWhitespace(c) // TODO: delegate this to the mode

  private val _edits = SeqBuffer[Buffer.Edit]()
  private val _actions = SeqBuffer[Action]()
  private val _redoEdits = SeqBuffer[Buffer.Edit]()
  private val _redoActions = SeqBuffer[Action]()
  private var _point = Loc(0, 0)
  private var _accumNextEdit = false
  private var _undoing = false
  private var _redoing = false
  private var _cleanUndoIdx = 0
  private var _cleanRedoIdx = -1
}

object UndoStack {

  /** Encapsulates an undoable group of buffer edits. */
  abstract class Action (val point :Loc) {
    /** Undoes the edits in the reverse of the order they were accumulated. */
    def undo () :Unit
    /** Requests to merge `edits` into this action. Returns true on success, false otherwise. */
    def accum (edits :SeqV[Buffer.Edit]) :Boolean
  }

  /** Creates the appropriate action for `edits`. */
  def toAction (point :Loc, edits :SeqV[Buffer.Edit]) :Action = edits.head match {
    case ins :Buffer.Insert if (edits.size == 1) => new SimpleInsert(point, ins)
    case _                                       => new General(point, edits.toSeq)
  }

  private class SimpleInsert (p :Loc, private[this] var edit :Buffer.Insert) extends Action(p) {
    def undo () = edit.undo()
    def accum (edits :SeqV[Buffer.Edit]) :Boolean = (edits.size == 1) && (edits.head match {
      case ins :Buffer.Insert if (ins.start == edit.end) => edit = edit.merge(ins) ; true
      case _ => false
    })
    override def toString = s"SimpleInsert($p, $edit)"
  }

  private class General (p :Loc, edits :Seq[Buffer.Edit]) extends Action(p) {
    def undo () = edits.reverse.foreach { _.undo() }
    def accum (edits :SeqV[Buffer.Edit]) = false
    override def toString = s"General($p, $edits)"
  }
}
