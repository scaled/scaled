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
  * removed) dynamically, but a dispatcher's major mode never changes.
  */
abstract class DispatcherImpl (editor :Editor, view :BufferViewImpl) extends Dispatcher {

  /** The major mode with which we interact. */
  val major :MajorMode = createMode()

  // because the major mode needs a dispatcher reference and the dispatcher needs a major mode
  // reference, this lives in an abstract field so that we can tie the Gordian knot
  protected def createMode () :MajorMode

  private var _curFn :String = _
  private var _prevFn :String = _
  override def curFn = _curFn
  override def prevFn = _prevFn

  override def completeFn (fnPre :String) = (Set[String]() /: _metas) {
    (fns, meta) => fns ++ meta.fns.complete(fnPre) }

  override def invoke (fn :String) = _metas.flatMap(_.fns.binding(fn)).headOption match {
    case Some(fn) => invoke(fn, "") ; true
    case None => false
  }

  override def press (trigger :String) {
    KeyPress.toKeyPresses(err => editor.emitStatus(s"Invalid trigger: $err"), trigger) match {
      case Some(kps) => resolve(kps, _metas) match {
        case Some(fn) => invoke(fn, kps.last.text)
        case None     => invokeMissed(trigger)
      }
      case None => editor.emitStatus(s"Unable to simulate press of $trigger")
    }
  }

  /* Disposes the major mode associated with this dispatcher and any active minor modes. */
  def dispose () {
    _metas map(_.mode) foreach(_.dispose())
    _metas = Nil // render ourselves useless
    _prefixes = Set()
  }

  /** Adds the specified minor mode to this dispatcher. */
  def addMode (minor :MinorMode) {
    _metas = new ModeMeta(minor) :: _metas
    rebuildPrefixes()
  }

  /** Removes the specified minior from this dispatcher. */
  def removeMode (minor :MinorMode) {
    val ometas = _metas
    _metas = _metas.filter(_.mode != minor)
    rebuildPrefixes()
    // if we actually removed this mode, dispose it
    if (ometas != _metas) minor.dispose()
  }

  private def rebuildPrefixes () :Unit = _prefixes = _metas.map(_.prefixes).reduce(_ ++ _)

  /** Processes the supplied key event, dispatching a fn if one is triggered thereby. */
  def keyPressed (kev :KeyEvent) :Unit = {
    kev.getEventType match {
      case KeyEvent.KEY_PRESSED =>
        // if this is a modifier key press, ignore it; wait for the modified key press
        if (isModifier(kev.getCode)) ()
        // if we get a new press when we're expecting a typed, then the user is holding down a
        // non-printing key (like an arrow key) and we're getting repeated presses; so handle
        // reporting the missed fn here
        else if (_dispatchTyped) invokeMissed()
        else {
          val key = KeyPress.fromPressed(kev).metafy(_escapeNext)
          // we handle ESC specially; it causes the next key press to be modified with meta
          if (key.id == "ESC" && !key.isModified) {
            deferDisplayPrefix(_trigger :+ key)
            _escapeNext = true
          }
          else {
            // tack this key onto our currently accumulating trigger
            _trigger :+= key
            // if it matches a known command prefix, then wait for the rest of the command to come in
            if (_prefixes(_trigger)) deferDisplayPrefix(_trigger)
            // otherwise resolve the fn bound to this trigger (if any)
            else resolve(_trigger, _metas) match {
              case Some(fn) => invoke(fn, _trigger.last.text)
              // if we don't find one, wait until the associated key typed event comes in
              case None     => _dispatchTyped = true
            }
          }
        }

      case KeyEvent.KEY_TYPED =>
        if (_dispatchTyped) {
          // create a press from the typed key (which will use the UTF8 character provided by the OS
          // rather than the character "printed on the key" to to speak); in this case we ignore
          // modifiers because the modifiers were interpreted by the OS to determine the typed key
          // (except META which seems not to influence the typed character; TODO: learn more)
          val typed = kev.getCharacter
          if (typed.length > 0 && !Character.isISOControl(typed.charAt(0))) {
            val key = new KeyPress(typed, typed, shift=false, ctrl=false, alt=false,
                                   meta=kev.isMetaDown).metafy(_escapeNext)
            _trigger = _trigger.dropRight(1) :+ key
          }
          val defFn = if (_trigger.size > 1 || _trigger.last.isModified) None else defaultFn
          resolve(_trigger, _metas) orElse defFn match {
            case Some(fn) => invoke(fn, _trigger.last.text)
            case None     => invokeMissed()
          }
        }

      case KeyEvent.KEY_RELEASED =>
        // if we get here and _dispatchTyped is true, that means we expected a KEY_TYPED event to
        // come in, but none did, so we need to treat the last key press like a missed fn
        if (_dispatchTyped) invokeMissed()
    }
    // consume all key events so that they don't percolate up and misbehave
    kev.consume()
  }

  /** Resolves the fn binding for a trigger sequence. */
  def resolve (trigger :Seq[KeyPress], modes :List[ModeMeta]) :Option[FnBinding] =
    // we could do modes.flatMap(_.map.get(trigger)).headOption but we want zero garbage
    if (modes.isEmpty) None else {
      val fnopt = modes.head.map.get(trigger)
      if (fnopt.isDefined) fnopt else resolve(trigger, modes.tail)
    }

  private def invoke (fn :FnBinding, typed :String) {
    // prepare to invoke our fn
    _curFn = fn.name
    editor.clearStatus()
    view.buffer.undoStack.actionWillStart(view.point())

    // TODO: pass view to fn for (internal) error reporting?
    fn.invoke(typed)

    // finish up after invoking our fn
    view.buffer.undoStack.actionDidComplete()
    _prevFn = _curFn
    _curFn = null
    didInvoke()
  }

  private def invokeMissed () :Unit = invokeMissed(_trigger.mkString(" "))
  private def invokeMissed (trigger :String) :Unit = missedFn match {
    case Some(fn) => invoke(fn, trigger)
    case None     => _prevFn = null ; didInvoke()
  }

  private def didInvoke () {
    _trigger = Seq()
    _dispatchTyped = false
    _escapeNext = false
  }

  /** Sets a timer that displays the current command prefix in the minibuffer after a short delay.
    * Thus if a user types a command prefix, we wait for the rest of the command, but we also
    * eventually provide some feedback as to what's going on in case they did it unwittingly. */
  private def deferDisplayPrefix (trigger :Seq[KeyPress]) {
    editor.emitStatus(trigger.mkString(" "))
  }

  private class ModeMeta (val mode :Mode) {
    val fns = new FnBindings(mode, editor.emitStatus)
    val map = DispatcherImpl.parseKeyMap(
      mode.keymap, fns,
      (key :String) => editor.emitStatus(s"Unknown key in keymap [mode=${mode.name}, key=$key]"),
      (fn :String) => editor.emitStatus(s"Unknown fn in keymap [mode=${mode.name}, fn=$fn]"))
    // enumerate all prefix sequences (we use these when processing key input)
    val prefixes :Set[Seq[KeyPress]] = map.keys.map(_.dropRight(1)).filter(!_.isEmpty).toSet
    // TODO: report an error if a key prefix is bound to an fn? WDED?
  }

  private val isModifier = Set(KeyCode.SHIFT, KeyCode.CONTROL, KeyCode.ALT, KeyCode.META,
                               KeyCode.COMMAND, KeyCode.WINDOWS)

  private val majorMeta = new ModeMeta(major)
  private val defaultFn :Option[FnBinding] = major.defaultFn.flatMap(majorMeta.fns.binding)
  private val missedFn :Option[FnBinding] = major.missedFn.flatMap(majorMeta.fns.binding)

  private var _metas = List(majorMeta) // the stack of active modes (major last)
  private var _prefixes = _metas.head.prefixes // union of cmd prefixes from active modes' keymaps

  private var _trigger = Seq[KeyPress]()
  private var _dispatchTyped = false
  private var _escapeNext = false
}

/** [[DispatcherImpl]] utilities. */
object DispatcherImpl {

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
