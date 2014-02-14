//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Defines an action to be taken when a key or series of keys is pressed. */
class KeyBinding private (_trigger :String, _action :AnyRef) {

  /** The triggering key or key sequence. A single key consists of the key identifier (e.g. 'g',
    * 'F1', '-',) prefixed by zero or more modifier keys ('C-' for control, 'M-' for meta, 'A-' for
    * alt, and 'S-' for shift). Key sequences consist of single keys separated by spaces.
    * Examples:
    *  - `e`: lowercase e
    *  - `S-s`: upper case S
    *  - `C-c`: control-c
    *  - `C-c C-i`: control-c followed by control-i
    */
  val trigger :String = _trigger

  /** The action to be taken when this key binding is triggered. This must be one of the supported
    * "action types", which currently only includes Scala's `FunctionN` classes, but will some day
    * be extended to support other languages' closure/function types.
    *
    * Key bindings may specify arbitrary dependencies from a limited set of supported types. These
    * dependencies will be bound when they are instantiated for a particular editing mode on a
    * particular buffer view.
    *
    * For example, a key binding which moves the cursor forward one character would require the
    * [[BufferView]] as a dependency:
    * {{{
    * key("C-f") (view :BufferView) => view.point = view.buffer.loc(view.point.offset+1)
    * }}}
    *
    * A binding can also depend on an active major or minor mode (generally the one that defined
    * it), and may also depend on multiple values:
    * {{{
    * key("M-.") (mode :ScalaMode, buffer :Buffer) => mode.navigateToSymbolAtPoint(buffer.point)
    * }}}
    *
    * Values supported as dependencies include:
    *  - the [[BufferView]] in which the key binding was triggered
    *  - the [[Buffer]] being edited by said `BufferView`
    *  - the [[Mode]] (major or minor) that is in effect when the binding is triggered
    *  - the [[Editor]] in which everything is running
    */
  val action :AnyRef = _action
}

/** [KeyBinding] constructors and related utilities. */
object KeyBinding {

  class Builder (trigger :String) {
    def apply (action : =>Unit) = new KeyBinding(trigger, () => action)
    def apply (fn :Function1[_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function2[_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function3[_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function4[_,_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function5[_,_,_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function6[_,_,_,_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function7[_,_,_,_,_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function8[_,_,_,_,_,_,_,_,_]) = new KeyBinding(trigger, fn)
    def apply (fn :Function9[_,_,_,_,_,_,_,_,_,_]) = new KeyBinding(trigger, fn)
  }

  def key (trigger :String) = new Builder(trigger)
}
