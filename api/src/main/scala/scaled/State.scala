//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.OptValue
import scala.collection.mutable.{Map => MMap}
import scala.reflect.ClassTag

/** Provides a read-only view of [[State]]. */
abstract class StateV {

  /** Returns the current state value associated with `key`, if any. */
  def get[T] (key :Class[T]) :Option[T]

  /** A `get` variant that uses class tags to allow usage like: `get[Foo]`. */
  def get[T] (implicit tag :ClassTag[T]) :Option[T]

  /** Returns the current state value associated with `key`.
    * @throws NoSuchElementException if no value is associated with `key`. */
  def req[T] (key :Class[T]) :T
}

/** Maintains a collection of type-keyed state. `Class` objects serve as the key for a piece of
  * state which maps to a reactive value which optionally holds an instance of the class. This is
  * used to maintain per-editor and per-buffer state maps.
  */
class State extends StateV {

  private val _states = MMap[Class[_],OptValue[_]]()

  /** Returns the state value associated with the specified type, if any. */
  def apply[T] (key :Class[T]) :OptValue[T] = _states.synchronized {
    _states.getOrElseUpdate(key, new OptValue[T](null.asInstanceOf[T]) {
      override def emptyFail = throw new NoSuchElementException(s"No state for $key")
    }).asInstanceOf[OptValue[T]]
  }

  /** An `apply` variant that uses class tags to allow usage like: `apply[Foo]`. */
  def apply[T] (implicit tag :ClassTag[T]) :OptValue[T] = apply(
    tag.runtimeClass.asInstanceOf[Class[T]])

  override def get[T] (key :Class[T]) = apply(key).getOption
  override def get[T] (implicit tag :ClassTag[T]) = apply(tag).getOption
  override def req[T] (key :Class[T]) = apply(key).get
}

/** Static [[State]] bits. */
object State {

  /** Used to provide initial state to a buffer. */
  class Init[T] (key :Class[T], value :T) {
    /** Applies this initial value to `state`. */
    def apply (state :State) :Unit = state(key).update(value)
  }

  /** Creates a state instance with the specified key and value. */
  def init[T] (key :Class[T], value :T) = new Init(key, value)

  /** Creates a state instance for a value with its class as key. */
  def init[T] (value :T) = new Init(value.getClass.asInstanceOf[Class[T]], value)
}
