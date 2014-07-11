//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.nio.file.Path
import scala.reflect.ClassTag

/** Scaled services must extend this class so that they can be notified of lifecycle events. */
abstract class AbstractService {

  /** A callback invoked when a service is first started by Scaled. */
  def didStartup () :Unit

  /** A callback invoked when a service is about to be shutdown by Scaled. */
  def willShutdown () :Unit
}

/** Provides services relating to services. */
@Service(name="service", impl="impl.ServiceManager",
         desc="Provides meta-services. Mainly logging and dependency injection.")
trait MetaService {

  /** Returns a `Path` for a file/directory with name `name` in the Scaled metadata directory. */
  def metaFile (name :String) :Path

  /** Returns a logger for reporting errors / non-UX feedback. */
  def log :Logger

  /** Returns an executor for doing things on different threads. */
  def exec :Executor

  /** Resolves and returns the Scaled service identified by `clazz`. `clazz` is a [[Service]]
    * annotated class, which will be created and initialized if it has not yet been so.
    * @throws InstantiationException if there is an error creating the service.  */
  def service[T] (clazz :Class[T]) :T

  /** A `service` variant that uses class tags to allow usage like: `service[Foo]`.
    * @throws InstantiationException if there is an error creating the service.  */
  def service[T] (implicit tag :ClassTag[T]) :T = service(tag.runtimeClass.asInstanceOf[Class[T]])

  /** Creates a process whose object is created by `thunk`. The creation of the process object takes
    * place in the execution context (thread) assigned to the process, not immediately. It will be
    * created the first time a request is made via the returned pipe. If immediate creation and
    * initialization is desired, follow this call with an immediate NOOP `tell`. */
  def process[P] (thunk : => P) :Pipe[P]

  /** Creates an instance of `clazz` via Scaled's dependency injection mechanism. `clazz` must have
    * a single public constructor.
    *
    * The arguments to that constructor are matched from `args` such that when a constructor
    * argument `a1` is seen, the first object in `args` which has compatible type is used. That
    * object is then removed from `args` and the process continues for constructor argument `a2`
    * and so forth. Additionally, if constructur argument is seen that is of some subtype of
    * `AbstractService`, then the appropriate service is resolved and used.
    *
    * It is not necessary for `args` to be completely consumed during this process. A service may
    * inject dependencies into its plugins when resolving them and may provide a variety of
    * optional arguments that the plugin may opt not to consume. Note, however, that due to the
    * fact that arguments are matched in order and by type, a plugin MUST consume the first argument
    * of a given type before it can consume subsequent arguments of that type. Services may wish
    * to take this restriction into account and provide unique types for injectable arguments
    * that might merely box a string or integer.
    */
  def injectInstance[T] (clazz :Class[T], args :List[Any]) :T
}
