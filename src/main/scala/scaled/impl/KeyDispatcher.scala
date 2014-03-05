//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.input.{KeyCode, KeyEvent}

import scaled._

/** Handles the conversion of key presses into execution of the appropriate fns. This includes
  * parsing text trigger sequences into efficient internal structures, mapping trigger sequences to
  * bound fns based on a mode's key bindings, and handling incoming key events and resolving them
  * into trigger sequences.
  *
  * @param mode the major mode which defines our primary key mappings. Minor modes are added (and
  * removed) dynamically, but if our major mode is changed we throw everything out and create a new
  * dispatcher.
  */
class KeyDispatcher (view :BufferViewImpl, mode :MajorMode) {

  class Metadata (mode :Mode) {
    val fns = new FnBindings(mode)
    val map = KeyDispatcher.parseKeyMap(
      mode.keymap, fns,
      (key :String) => view.emitStatus(s"Unknown key in keymap [mode=${mode.name}, key=$key]"),
      (fn :String) => view.emitStatus(s"Unknown fn in keymap [mode=${mode.name}, fn=$fn]"))

    // TODO: enumerate all prefix sequences (we'll need to know the union of those when processing
    // key input)
  }

  def keyPressed (kev :KeyEvent) :Unit = kev.getEventType match {
    case KeyEvent.KEY_PRESSED =>
      // TODO: accumulate modified keys into a trigger until something matches
      val trigger = Seq(KeyPress(kev))
      // look through our stack of keymaps and pick the first one that matches
      _metas.map(_.map.get(trigger)).collectFirst { case Some(fn) => fn } match {
        case Some(fn) =>
          _insertNext = false
          view.undoStack.actionWillStart()
          fn.invoke() // TODO: pass log to fn for error reporting
          view.undoStack.actionDidComplete()
          kev.consume()
        case None =>
          // if they pressed a modified key (held ctrl/alt/meta and pressed a key) and we don't
          // have a mapping for it, issue a warning since they probably meant it to do something
          if (isModified(kev) && !isModifier(kev.getCode)) {
            // TODO: turn keyCode into a key description
            view.emitStatus(s"${KeyPress(kev)} is undefined.")
          }
          // unmodified key presses that don't map to a fn should be inserted into the buffer,
          // but we won't know what character to insert until the associated KEY_TYPED event
          // comes in
          _insertNext = !isModified(kev)
      }

    case KeyEvent.KEY_TYPED =>
      // if the KEY_PRESSED event that necessarily preceded this KEY_TYPED event indicated that
      // the key should be inserted into the buffer, do so (assuming the key maps to a char)
      if (_insertNext && kev.getCharacter != KeyEvent.CHAR_UNDEFINED) {
        view.undoStack.actionWillStart()
        // insert the typed character at the point
        val p = view.point
        view.buffer.insert(p, kev.getCharacter)
        // move the point to the right by the appropriate amount
        view.point = p + (0, kev.getCharacter.length)
        // TODO: should the above be built-into BufferView?
        view.undoStack.actionDidComplete()
      }

    case _ => // key released, don't care
  }

  private def isModified (kev :KeyEvent) = kev.isControlDown || kev.isAltDown || kev.isMetaDown

  private var _metas = Seq(new Metadata(mode))
  private var _insertNext = false

  private val isModifier = Set(KeyCode.SHIFT, KeyCode.CONTROL, KeyCode.ALT, KeyCode.META,
                               KeyCode.COMMAND, KeyCode.WINDOWS)
}

/** [[KeyDispatcher]] utilities. */
object KeyDispatcher {

  /** Parses a keymap description, resolving the trigger sequences into `Seq[KeyPress]` and the fn
    * bindings to `FnBinding` from `fns`. Invalid mappings are omitted from the results after
    * notifying one or both of the supplied invalidity callbacks.
    *
    * @param onInvalidKey a callback to be notified when an invalid trigger sequence is encountered.
    * @param onInvalidFn a callback to be notified when an invalid fn binding is encountered.
    */
  def parseKeyMap (keymap :Seq[(String,String)], fns :FnBindings, onInvalidKey :String => Unit,
                   onInvalidFn :String => Unit) :Map[Seq[KeyPress], FnBinding] = {
    Map() ++ keymap flatMap {
      case (key, fn) => (KeyPress.toKeyPresses(onInvalidKey, key), fns.binding(fn)) match {
        case (_, None)            => onInvalidFn(fn)   ; None
        case (None, Some(fb))     => None
        case (Some(kp), Some(fb)) => Some(kp -> fb)
      }
    }
  }
}
