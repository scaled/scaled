//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

/** A box that holds an `AutoCloseable` which is created on demand and closed and released when the
  * box itself is closed.
  */
abstract class CloseBox[C >: Null <: AutoCloseable] extends AutoCloseable {

  private var _contents :C = null

  /** Creates (if necessary) and returns the contents of this box. */
  def get :C = {
    if (_contents == null) {
      _contents = create()
      didCreate()
    }
    _contents
  }

  /** Closes the contents of this box (if they have been created) and nulls out the reference.
    * Subsequent calls to [[get]] will cause the contents to be recreated. */
  def close () {
    if (_contents != null) {
      try _contents.close()
      finally _contents = null
    }
  }

  /** Creates the contents of this box. */
  protected def create () :C

  /** Called when our contents have been created. */
  protected def didCreate () {}
}
