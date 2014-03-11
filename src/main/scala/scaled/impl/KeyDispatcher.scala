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
class KeyDispatcher (view :BufferViewImpl, major :MajorMode) {

  class Metadata (mode :Mode) {
    val fns = new FnBindings(mode, view.emitStatus)
    val map = KeyDispatcher.parseKeyMap(
      mode.keymap, fns,
      (key :String) => view.emitStatus(s"Unknown key in keymap [mode=${mode.name}, key=$key]"),
      (fn :String) => view.emitStatus(s"Unknown fn in keymap [mode=${mode.name}, fn=$fn]"))

    // TODO: enumerate all prefix sequences (we'll need to know the union of those when processing
    // key input)
  }

  /** Processes the supplied key event, dispatching a fn if one is triggered thereby. */
  def keyPressed (kev :KeyEvent) :Unit = kev.getEventType match {
    case KeyEvent.KEY_PRESSED =>
      // TODO: accumulate modified keys into a trigger prefix while said modified keys match a
      // trigger prefix known to exist in our keymaps
      val key = KeyPress.fromPressed(kev)
      val trigger = Seq(key)
      resolve(trigger, _metas) match {
        case Some(fn) => _deferredKey = null ; invoke(trigger, fn)
        // if we don't have a fn for this key-press-based trigger; try again when the associated
        // key typed event comes in (if any); we'll build a new trigger based on the typed
        // character and that may match, or it may be a candidate for default-fn dispatch
        case None     => _deferredKey = key
      }

    case KeyEvent.KEY_TYPED =>
      if (_deferredKey != null) {
        // create a press from the typed key (which will use the UTF8 character provided by the OS
        // rather than the character "printed on the key" to to speak); in this case we ignore
        // modifiers because the modifiers were interpreted by the OS to determine the typed key
        // (except META which seems not to influence the typed character; TODO: learn more)
        val typed = kev.getCharacter
        val key = if (typed.length == 0 || Character.isISOControl(typed.charAt(0))) _deferredKey
                  else KeyPress(typed, shift=false, ctrl=false, alt=false, meta=kev.isMetaDown)
        val trigger = Seq(key) // TODO: proper trigger
        val defFn = if (key.isModified) None else defaultFn
        resolve(trigger, _metas) orElse defFn match {
          case Some(fn) => invoke(trigger, fn)
          case None =>
            view.emitStatus(s"${trigger.mkString(" ")} is undefined.")
            view.didMissFn() // let the view know that we executed an "error fn"
        }
        // TODO: reset _dispatchTyped? can we have two TYPED events without an intervening PRESSED
        // event? I don't think so, but I may be wrong
      }

    case _ => // key released, don't care
  }

  /** Resolves the fn binding for a trigger sequence. Mainly a helper for [[keyPressed]]. */
  def resolve (trigger :Seq[KeyPress], modes :List[Metadata]) :Option[FnBinding] = modes match {
    case Nil     => None
    case m :: ms => m.map.get(trigger) match {
      case None   => resolve(trigger, ms)
      case somefn => somefn
    }
  }

  /** Dispatches the supplied trigger sequence to `fn`. */
  def invoke (trigger :Seq[KeyPress], fn :FnBinding) {
    view.willExecFn(fn)
    fn.invoke(trigger.last.text) // TODO: pass view to fn for error reporting?
    view.didExecFn(fn)
  }

  private val isModifier = Set(KeyCode.SHIFT, KeyCode.CONTROL, KeyCode.ALT, KeyCode.META,
                               KeyCode.COMMAND, KeyCode.WINDOWS)

  private val majorMeta = new Metadata(major)
  private val defaultFn :Option[FnBinding] = major.defaultFn.flatMap(majorMeta.fns.binding)

  private var _metas = List(majorMeta)
  private var _deferredKey :KeyPress = _
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
