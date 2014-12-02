//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.util.IdentityHashMap
import scaled.Logger

/** A trait for use by manually managed resources. */
trait Reffed {

  /** A bag of closeables that will be closed when this instance hibernates. */
  val toClose = Close.bag()

  /** Notes that `ref` is now using this instance.
    * @return a handle that should be used to release the reference. */
  def reference (ref :Any) :AutoCloseable = {
    val handle = new AutoCloseable() {
      override def close () {
        if (_refs.remove(this) == null) log.log(s"double release: $this!")
        else {
          log.log(s"release: $this") // TEMP: debug
          if (_refs.isEmpty()) hibernate()
        }
      }
      override def toString = s"${Reffed.this} <= $ref"
    }
    _refs.put(handle, ref)
    log.log(s"ref: $handle") // TEMP: debug
    handle
  }

  /** Returns the number of active references to this instance. */
  def references :Int = _refs.size
  private val _refs = new IdentityHashMap[Any,Any]()

  protected def hibernate () {
    println(s"$this hibernating")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this hibernate failure", e)
    }
  }

  protected def log :Logger
}
