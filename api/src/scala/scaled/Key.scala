//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.annotation.varargs

object Key {

  /** Defines a key binding. Trigger sequences are defined thusly: A single key consists of the key
    * identifier (e.g. 'g', 'F1', '-',) prefixed by zero or more modifier keys ('C-' for control,
    * 'M-' for meta, 'A-' for alt, and 'S-' for shift). Key sequences consist of single keys
    * separated by spaces. Examples:
    *
    *  - `e`: lowercase e
    *  - `S-s`: upper case S
    *  - `C-c`: control-c
    *  - `C-c C-i`: control-c followed by control-i
    *
    * The fn bindings are defined by the mode, by using the [[Fn]] annotation on methods. The name
    * in the keymap corresponds to the de-camel-cased method name (see [[Mode]] docs). When a mode
    * refers to its own fns, it may provide just the name, but if a mode (or a mode hook) refers to
    * another mode's fns, it must prefix the name by the name of the mode and a colon (e.g.
    * "scala:goto-term").
    */
  case class Binding (trigger :String, fn :String)

  /** Used to build up the key map for a mode. */
  class Map {
    private val _bindings = SeqBuffer[Binding]()

    /** Returns the bindings in this map. */
    def bindings :SeqV[Binding] = _bindings

    /** Appends a binding to this keymap. */
    @varargs def bind (fn :String, triggers :String*) :this.type = {
      triggers foreach { tr => _bindings += Binding(tr, fn) }
      this
    }

    /** Clears any existing bindings. */
    def clear () :this.type = {
      _bindings.clear()
      this
    }
  }
}
