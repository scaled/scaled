//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.reflect.ClassTag
import scaled.util.{BufferBuilder, Describable, Errors}

/** Provides a read-only view of buffer state. See [[State]]. */
abstract class StateV extends Describable {

  /** Returns the current state value associated with `key`, if any. */
  def get[T] (key :Class[T]) :Option[T]

  /** A `get` variant that uses class tags to allow usage like: `get[Foo]`. */
  def get[T] (implicit tag :ClassTag[T]) :Option[T]
  // note: we can't implement this here or Scala's overload resolution chokes... sigh

  /** Returns the current state value associated with `key`.
    * @throws NoSuchElementException if no value is associated with `key`. */
  def req[T] (key :Class[T]) :T

  /** A `req` variant that uses class tags to allow usage like: `req[Foo]`. */
  def req[T] (implicit tag :ClassTag[T]) :T = req(tag.runtimeClass.asInstanceOf[Class[T]])

  /** Returns the current state value associated with `key`.
    * @throws FeedbackException with `msg` no value is associated with `key`. */
  def req[T] (key :Class[T], msg :String) :T

  /** A `req` variant that uses class tags to allow usage like: `req[Foo](msg)`. */
  def req[T] (msg :String)(implicit tag :ClassTag[T]) :T =
    req(tag.runtimeClass.asInstanceOf[Class[T]], msg)

  /** Returns the keys for all currently defined state. */
  def keys :Set[Class[_]]

  /** Adds a description of this state to `bb`. */
  def describeSelf (bb :BufferBuilder) {
    describeState(bb)
    describeDescribables(bb)
  }

  /** Adds a description of just this state to `bb`. */
  def describeState (bb :BufferBuilder) {
    val kvs = keys.toSeq.flatMap(k => get(k).map(_.toString).map(v => (s"${k.getName}: " -> v)))
    if (!kvs.isEmpty) {
      bb.addSection("State")
      bb.addKeysValues(kvs)
    }
  }

  /** Adds a description of `Describable` state to `bb`. */
  def describeDescribables (bb :BufferBuilder) {
    keys.toSeq.flatMap(k => get(k)).collect({ case d :Describable => d }).
      foreach(_.describeSelf(bb))
  }
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

  /** A signal emitted when a new type of state is added. */
  val keyAdded = Signal[Class[_]]()

  /** A signal emitted when a type of state that was mapped is cleared. */
  val keyCleared = Signal[Class[_]]()

  /** Returns the state value associated with the specified type, if any. */
  def apply[T] (key :Class[T]) :OptValue[T] = _states.get(key).asInstanceOf[OptValue[T]]

  /** An `apply` variant that uses class tags to allow usage like: `apply[Foo]`. */
  def apply[T] (implicit tag :ClassTag[T]) :OptValue[T] =
    apply(tag.runtimeClass.asInstanceOf[Class[T]])

  /** Returns the keys for all currently defined state. */
  def keys :Set[Class[_]] = _states.asMap.keySet.filter(key => apply(key).isDefined).toSet

  override def get[T] (key :Class[T]) = apply(key).getOption
  override def get[T] (implicit tag :ClassTag[T]) = apply(tag).getOption
  override def req[T] (key :Class[T]) = apply(key).get
  override def req[T] (key :Class[T], msg :String) = {
    val opt = apply(key)
    if (opt.isDefined) opt.get
    else throw Errors.feedback(msg)
  }
  override def set[T] (key :Class[T], value :T) :Unit = apply(key).update(Some(value))
  override def clear[T] (key :Class[T]) :Unit = apply(key).update(None)

  override def toString = keys.map(k => s"$k=${apply(k)}").toString

  private val _states = Mutable.cacheMap { key :Class[_] =>
    val opt = new OptValue[Any](null) {
      override def emptyFail = throw new NoSuchElementException(s"No state for $key")
    }
    opt.onChange((nval, oval) => {
      if (nval.isDefined && !oval.isDefined) keyAdded.emit(key)
      else if (!nval.isDefined && oval.isDefined) keyCleared.emit(key)
    })
    opt
  }
  inits foreach { _.apply(this) }
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
