//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.util.control.NonFatal

/** Represents the result of a computation or failure. Instances are either [[Success]] or
  * [[Failure]].
  *
  * Adapted from Scala's `Try`: made to use Java's HOF types and pruned somewhat.
  */
sealed abstract class Try[+T] {

  /** Returns `true` if the `Try` is a `Failure`, `false` otherwise. */
  def isFailure: Boolean

  /** Returns `true` if the `Try` is a `Success`, `false` otherwise. */
  def isSuccess: Boolean

  /** Returns the value from this `Success` or throws the exception if this is a `Failure`. */
  def get: T

  /** Applies the given function `f` if this is a `Success`, otherwise returns `Unit` if this is a
    * `Failure`. ''Note:'' If `f` throws, then this method may throw an exception. */
  def foreach (f :JConsumer[T]) :Unit

  /** Returns the given function applied to the value from this `Success` or returns this if this
    * is a `Failure`. */
  def flatMap[U] (f :JFunction[T, Try[U]]) :Try[U]

  /** Maps the given function to the value from this `Success` or returns this if this is a
    * `Failure`. */
  def map[U] (f :JFunction[T, U]) :Try[U]

  /** Applies the given partial function to the value from this `Success` or returns this if this
    * is a `Failure`. */
  def collect[U] (pf :PartialFunction[T, U]) :Try[U]

  /** Completes this `Try` by applying the function `f` to this if this is of type `Failure`, or
    * conversely, by applying `s` if this is a `Success`. */
  def transform[U] (s :JFunction[T, Try[U]], f :JFunction[Throwable, Try[U]]) :Try[U]

  /** Applies `fa` if this is a `Failure` or `fb` if this is a `Success`. If `fb` is initially
    * applied and throws an exception, then `fa` is applied with this exception. */
  def fold[U] (fa :JFunction[Throwable, U], fb :JFunction[T, U]) :U
}

final case class Failure[+T] (exception :Throwable) extends Try[T] {
  override def isFailure = true
  override def isSuccess = false
  override def get :T = throw exception
  override def flatMap[U] (f :JFunction[T, Try[U]]) :Try[U] = this.asInstanceOf[Try[U]]
  override def foreach (f :JConsumer[T]) :Unit = ()
  override def transform[U] (s :JFunction[T, Try[U]], f :JFunction[Throwable, Try[U]]) :Try[U] =
    try f(exception) catch { case NonFatal(e) => Failure(e) }
  override def map[U] (f :JFunction[T, U]) :Try[U] = this.asInstanceOf[Try[U]]
  override def collect[U] (pf :PartialFunction[T, U]) :Try[U] = this.asInstanceOf[Try[U]]
  override def fold[U] (fa :JFunction[Throwable, U], fb :JFunction[T, U]) :U = fa(exception)
}

final case class Success[+T] (value :T) extends Try[T] {
  override def isFailure = false
  override def isSuccess = true
  override def get = value
  override def flatMap[U] (f :JFunction[T, Try[U]]) :Try[U] =
    try f(value) catch { case NonFatal(e) => Failure(e) }
  override def foreach (f :JConsumer[T]) :Unit = f.accept(value)
  override def transform[U] (s :JFunction[T, Try[U]], f :JFunction[Throwable, Try[U]]) :Try[U] =
    this flatMap s
  override def map[U] (f :JFunction[T, U]) :Try[U] =
    try Success(f(value)) catch { case NonFatal(e) => Failure(e) }
  override def collect[U] (pf :PartialFunction[T, U]) :Try[U] =
    try {
      if (pf isDefinedAt value) Success(pf(value))
      else Failure(new NoSuchElementException("Predicate does not hold for " + value))
    } catch { case NonFatal(e) => Failure(e) }
  override def fold[U] (fa :JFunction[Throwable, U], fb :JFunction[T, U]) :U =
    try { fb(value) } catch { case NonFatal(e) => fa(e) }
}
