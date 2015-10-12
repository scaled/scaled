//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.reflect.ClassTag

/** Provides a read-only view of buffer state. See [[State]]. */
abstract class StateV {

  /** Returns the current state value associated with `key`, if any. */
  def get[T] (key :Class[T]) :Option[T]

  /** A `get` variant that uses class tags to allow usage like: `get[Foo]`. */
  def get[T] (implicit tag :ClassTag[T]) :Option[T]

  /** Returns the current state value associated with `key`.
    * @throws NoSuchElementException if no value is associated with `key`. */
  def req[T] (key :Class[T]) :T

  /** Returns the keys for all currently defined state. */
  def keys :Set[Class[_]]
}

/** Provides a mutable, but non-reactive view of buffer state. See [[RState]]. */
abstract class State extends StateV {

  /* Sets the state value associated with `key` to `value`. */
  def set[T] (key :Class[T], value :T) :Unit

  /* A `set` variant that uses class tags to allow usage like: `set[Foo](value)`. */
  def set[T] (value :T)(implicit tag :ClassTag[T]) :Unit =
    set(tag.runtimeClass.asInstanceOf[Class[T]], value)

  /** Clears the current state value associated with `key`. */
  def clear[T] (key :Class[T]) :Unit

  /** A `clear` variant that uses class tags to allow usage like: `clear[Foo]`. */
  def clear[T] (implicit tag :ClassTag[T]) :Unit = clear(tag.runtimeClass.asInstanceOf[Class[T]])
}

/** Maintains a collection of type-keyed state. `Class` objects serve as the key for a piece of
  * state which maps to a reactive value which optionally holds an instance of the class. This is
  * used to maintain per-editor and per-buffer state maps.
  */
class RState (inits :State.Init[_]*) extends State {

  private val _states = Mutable.cacheMap { key :Class[_] => new OptValue[Any](null) {
    override def emptyFail = throw new NoSuchElementException(s"No state for $key")
  }}
  inits foreach { _.apply(this) }

  /** Returns the state value associated with the specified type, if any. */
  def apply[T] (key :Class[T]) :OptValue[T] = _states.get(key).asInstanceOf[OptValue[T]]

  /** An `apply` variant that uses class tags to allow usage like: `apply[Foo]`. */
  def apply[T] (implicit tag :ClassTag[T]) :OptValue[T] = apply(
    tag.runtimeClass.asInstanceOf[Class[T]])

  /** Returns the keys for all currently defined state. */
  def keys :Set[Class[_]] = _states.asMap.keySet.toSet

  override def get[T] (key :Class[T]) = apply(key).getOption
  override def get[T] (implicit tag :ClassTag[T]) = apply(tag).getOption
  override def req[T] (key :Class[T]) = apply(key).get
  override def set[T] (key :Class[T], value :T) :Unit = apply(key).update(Some(value))
  override def clear[T] (key :Class[T]) :Unit = apply(key).update(None)
}

/** Static [[State]] bits. */
object State {

  /** Used to provide initial state to a buffer. */
  class Init[T] (key :Class[T], value :T) {
    /** Applies this initial value to `state`. */
    def apply (state :RState) :Unit = state(key).update(value)
  }

  /** Creates a state instance with the specified key and value. */
  def init[T] (key :Class[T], value :T) = new Init(key, value)

  /** Creates a state instance for a value with its class as key. */
  def init[T] (value :T) = new Init(value.getClass.asInstanceOf[Class[T]], value)

  /** Creates a state instance for a list of values, each with its class as key. */
  def inits (values :Any*) :List[State.Init[_]] = {
    val lb = List.builder[State.Init[_]]()
    values foreach { lb += init(_) }
    lb.build()
  }
}
