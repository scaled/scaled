//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Handles the invocation of fns and their binding to names and key combinations. */
abstract class Dispatcher {

  /** The name of the currently executing fn. Is null when no fn is executing. */
  def curFn :String

  /** The name of the previously executed fn. Will be null until at least one fn has been executed.
    * Used by certain fns to specialize their behavior when invoked repeatedly, and for other
    * nefarious reasons. */
  def prevFn :String

  /** Returns the set of all fns whose name starts with `fnPre`. */
  def completeFn (fnPre :String) :Set[String]

  /** Returns the documentation for `fn` if such fn exists. */
  def describeFn (fn :String) :Option[String]

  /** Returns the set of all major modes whose name starts with `modePre`. */
  def completeMajor (modePre :String) :Set[String]

  /** Returns the set of all minor modes whose name starts with `modePre`. */
  def completeMinor (modePre :String) :Set[String]

  /** Returns a list of all active modes. */
  def modes :List[Mode]

  /** Toggles the activation of the minor mode named `mode`. */
  def toggleMode (mode :String) :Unit

  /** Resolves the fn named `fn`. If found, it is invoked and `true` is returned. If no fn is found
    * with that name, `false` is returned. */
  def invoke (fn :String) :Boolean

  /** Simulates the press of the key sequence represented by `trigger` (e.g. `C-x C-c`). The
    * associated `fn` is resolved and invoked, or the major mode's `missedFn` is invoked if no
    * bound `fn` could be found. */
  def press (trigger :String) :Unit
}
