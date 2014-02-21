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
class KeyDispatcher (view :BufferView, mode :MajorMode) {

  class Metadata (mode :Mode) {
    val fns = new FnBindings(mode)
    val map = KeyDispatcher.parseKeyMap(
      mode.keymap, fns,
      (key :String) => view.emitStatus(s"Unknown key in keymap [mode=${mode.name}, key=$key]"),
      (fn :String) => view.emitStatus(s"Unknown fn in keymap [mode=${mode.name}, fn=$fn]"))

    // TODO: enumerate all prefix sequences (we'll need to know the union of those when processing
    // key input)
  }

  def keyPressed (kev :KeyEvent) {
    // println(kev)
    kev.getEventType match {
      case KeyEvent.KEY_PRESSED =>
        val trigger = Seq(KeyPress(kev))
        _metas.map(_.map.get(trigger)).collectFirst {
          case Some(fn) => fn.invoke() ; kev.consume()
        }
      case KeyEvent.KEY_TYPED =>
        // TEMP: insert typed characters into the buffer at the point
        if (!(kev.isControlDown || kev.isMetaDown || kev.isAltDown ||
          kev.getCharacter == KeyEvent.CHAR_UNDEFINED)) {
          // insert the typed character
          view.buffer.line(view.point).insert(view.point.col, kev.getCharacter)
          // move the point to the right by the appropriate amount
          view.point = view.point + (0, kev.getCharacter.length)
          // TODO: should the above be built-into BufferView?
        }
      case _ => // key released, don't care
    }
  }

  private var _metas = Seq(new Metadata(mode))
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
        case (None, None)         => onInvalidKey(key) ; onInvalidFn(fn) ; None
        case (Some(kp), None)     => onInvalidFn(fn)   ; None
        case (None, Some(fb))     => onInvalidKey(key) ; None
        case (Some(kp), Some(fb)) => Some(kp -> fb)
      }
    }
  }
}
