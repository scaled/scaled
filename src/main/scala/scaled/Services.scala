//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Scaled services must extend this class so that they can be notified of lifecycle events. */
abstract class AbstractService {

  /** A callback invoked when a service is first started by Scaled. */
  def didStartup () :Unit

  /** A callback invoked when a service is about to be shutdown by Scaled. */
  def willShutdown () :Unit
}

/** Provides services relating to services. */
@Service(name="service", impl="impl.ServiceManager",
         desc="Provides meta-services. Mainly dependency injection.")
trait MetaService {

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
