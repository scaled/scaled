//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.reflect.ClassTag

/** Enables the dispatch of events to loosely coupled listeners. An event is simply a POJO
  * (conventionally named `FooEvent`). Listeners are registered on the type of the event class
  * (mechanisms exist to register on a supertype and listen on a subtype).
  *
  * Different event busses exist at different "scopes" (for example, global, workspace-wide,
  * editor-wide). An event should naturally lend itself to a particular scope. For example, an
  * event indicating that a buffer was opened in an editor would naturally be dispatched on the
  * editor's event bus.
  */
class EventBus {

  /** Returns the signal used to emit and listen for events of type `T`. */
  def signal[T] (eclass :Class[T]) :Signal[T] = _sigs.get(eclass).asInstanceOf[Signal[T]]

  /** A `signal` variant that uses class tags to allow usage like: `signal[Foo]`. */
  def signal[T] (implicit tag :ClassTag[T]) :Signal[T] =
    signal(tag.runtimeClass.asInstanceOf[Class[T]])

  private val _sigs = Mutable.cacheMap { _ :Class[_] => Signal.apply[Object]() }
}
