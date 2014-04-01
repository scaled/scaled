//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

import scala.collection.mutable.ArrayBuffer

import reactual.Connection

/** Encapsulates a reactive behavior and simplifies the process of wiring up a bunch of reactions
  * when the behavior is enabled and clearing those reactions when the behavior is disabled.
  */
abstract class Behavior {

  /** Activate or deactivates this behavior, as appropriate. */
  def setActive (active :Boolean) {
    if (!active) {
      if (!_conns.isEmpty) {
        _conns foreach { _.close() }
        _conns.clear()
        didDeactivate()
        assert(_conns.isEmpty, "New connections added in didDeactivate()!?")
      }
    } else if (_conns.isEmpty) {
      activate()
      assert(!_conns.isEmpty, "Behaviors must note at least one connection in activate().")
    }
  }

  /** Wires up all reactions in this method and performs any other activation processing. Be sure to
    * use [[note]] to note all connections created when wiring up reactions */
  protected def activate () :Unit

  /** Called after this behavior has been deactivated. */
  protected def didDeactivate () {}

  /** Notes a reactive connection. The connection will be closed on the next call to
    * [[setActive]]`(false)`. This should only be called from [[activate]]. */
  protected def note (conn :Connection) {
    _conns += conn
  }

  private val _conns = ArrayBuffer[Connection]()
}
