//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** A "refinement" of [[AutoCloseable]] that does not throw exceptions on [[close]]. */
trait Closeable extends AutoCloseable {

  /** Performs the deferred close operation. */
  def close () :Unit
}

object Closeable {

  /** A closeable that does nothing. Simplifies situations where you close an old closeable and
    * replace it with a new one. */
  val Noop :Closeable = new Closeable() {
    override def close () :Unit = {}
  }

  /** Creates a [[Closeable]] that invokes `thunk` when `close` is called. */
  def apply[U] (thunk : => U) = new Closeable() {
    def close () = thunk
  }
}
