//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

/** Encapsulates a reactive behavior and simplifies the process of wiring up a bunch of reactions
  * when the behavior is enabled and clearing those reactions when the behavior is disabled.
  */
abstract class Behavior extends AutoCloseable {

  /** Activate or deactivates this behavior, as appropriate. */
  def setActive (active :Boolean) {
    if (!active) {
      if (!_toClose.isEmpty) {
        _toClose.close()
        didDeactivate()
        assert(_toClose.isEmpty, "New connections added in didDeactivate()!?")
      }
    } else if (_toClose.isEmpty) {
      activate()
      assert(!_toClose.isEmpty, "Behaviors must note at least one connection in activate().")
    }
  }

  /** Deactivates this behavior on close. */
  override def close () :Unit = setActive(false)

  /** Wires up all reactions in this method and performs any other activation processing. Be sure to
    * use [[note]] to note all connections created when wiring up reactions */
  protected def activate () :Unit

  /** Called after this behavior has been deactivated. */
  protected def didDeactivate () {}

  /** Notes a closeable resource. The closeable will be closed on the next call to
    * [[setActive]]`(false)`. This should only be called from [[activate]]. */
  protected def note (ac :AutoCloseable) {
    _toClose += ac
  }

  private val _toClose = Close.bag()
}
