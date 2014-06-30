//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

/** Encapsulates a reactive behavior and simplifies the process of wiring up a bunch of reactions
  * when the behavior is enabled and clearing those reactions when the behavior is disabled.
  */
abstract class Behavior extends AutoCloseable {

  /** Activate or deactivates this behavior, as appropriate. */
  def setActive (active :Boolean) {
    if (!active) deactivate(false)
    else if (_toClose.isEmpty) {
      activate()
      assert(!_toClose.isEmpty, "Behaviors must note at least one connection in activate().")
    }
  }

  /** Deactivates this behavior on close. Note that is only expected to be called when the buffer
    * with which our mode is associated is being disposed as well. If the buffer is to remain, we
    * expect the mode to deactivate us explicitly before we're closed by virtue of being an
    * auto-closeable registered with the mode. */
  override def close () :Unit = setActive(true)

  /** Wires up all reactions in this method and performs any other activation processing. Be sure to
    * use [[note]] to note all connections created when wiring up reactions. */
  protected def activate () :Unit

  /** Called after this behavior has been deactivated.
    * @param bufferDisposing if true the buffer that contains this behavior will be disposed
    * immediately after this behavior is deactivated. Thus the behavior may avoid unnecessary work
    * by not removing styles or doing other work it might normally do if it were deactivated and the
    * buffer were to remain operable. */
  protected def didDeactivate (bufferDisposing :Boolean) {}

  /** Notes a closeable resource. The closeable will be closed on the next call to
    * [[setActive]]`(false)`. This should only be called from [[activate]]. */
  protected def note (ac :AutoCloseable) {
    _toClose += ac
  }

  private def deactivate (bufferDisposing :Boolean) {
    if (!_toClose.isEmpty) {
      _toClose.close()
      didDeactivate(bufferDisposing)
      assert(_toClose.isEmpty, "New connections added in didDeactivate()!?")
    }
  }

  private val _toClose = Close.bag()
}
