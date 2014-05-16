//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scala.collection.mutable.ArrayBuffer

/** Maintains a list of [[AutoCloseable]]s and simplifies the process of closing them all at once.
  * Note: this list is not thread-safe.
  */
class CloseList extends AutoCloseable {

  private val _acs = ArrayBuffer[AutoCloseable]()

  /** Returns true if we have zero closeables in our list. */
  def isEmpty :Boolean = _acs.isEmpty

  /** Adds `ac` to this close list. */
  def add (ac :AutoCloseable) :Unit = _acs += ac
  /** Adds `ac` to this close list. */
  def += (ac :AutoCloseable) :Unit = add(ac)

  /** Removes `ac` from this close list. */
  def remove (ac :AutoCloseable) :Unit = _acs -= ac
  /** Removes `ac` from this close list. */
  def -= (ac :AutoCloseable) :Unit = remove(ac)

  /** Closes all closeables in this list and clears it. If any exceptions occur during closure,
    * they are accumulated into single [[RuntimeException]] as suppressed exceptions. */
  def close () {
    var exn = null :RuntimeException
    val iter = _acs.iterator ; while (iter.hasNext) {
      try iter.next.close()
      catch {
        case t :Throwable =>
          if (exn == null) exn == new RuntimeException("Close failure(s).")
          exn.addSuppressed(t)
      }
    }
    _acs.clear()
    if (exn != null) throw exn
  }
}
