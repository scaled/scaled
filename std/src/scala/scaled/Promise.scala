//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

import scala.util.{Try, Success, Failure}

/** Provides a concrete implementation of [[Future]] that can be updated with a success or failure
  * result when it becomes available.
  *
  * This implementation also guarantees a useful behavior, which is that all listeners added prior
  * to the completion of the promise will be cleared when the promise is completed, and no further
  * listeners will be retained. This allows the promise to be retained after is has been completed
  * as a useful "box" for its underlying value, without concern that references to long satisfied
  * listeners will be inadvertently retained.
  */
class Promise[T] private (_result :Value[Try[T]]) extends Future[T](_result) {

  /** Creates an uncompleted promise. */
  def this () = this(new Value[Try[T]](null) {
    override protected def updateAndNotify (value :Try[T], force :Boolean) = synchronized {
      if (get != null) throw new IllegalStateException("Already completed")
      try {
        super.updateAndNotify(value, force)
      } finally {
        clearListeners() // clear out our listeners now that they have been notified
      }
    }
  })

  /** Causes this promise to be completed successfully with `value`. */
  def succeed (value :T) :Unit = _result.update(Success(value))

  /** Causes this promise to be completed with failure caused by `cause`. */
  def fail (cause :Throwable) :Unit = _result.update(Failure(cause))

  /** Returns true if there are listeners awaiting the completion of this promise. */
  def hasConnections :Boolean = _result.hasConnections
}

object Promise {

  /** Creates a promise with type `T`. */
  def apply[T] () :Promise[T] = new Promise[T]()
}
