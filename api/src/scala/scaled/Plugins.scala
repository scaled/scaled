//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Scaled plugins must extend this class and have a name of the form `FooPlugin`. */
abstract class AbstractPlugin extends AutoCloseable {

  /** Called when this plugin is unloaded. This happens either when the plugin set of which it is a
    * part is closed, or when the package that defines this plugin is removed. */
  override def close () {}
}

/** Manages the set of plugins that match a particular tag. A service that's making use of plugins
  * will obtain a plugin set for a particular tagged plugin and can then simply access [[plugins]]
  * when they need to enumerate all plugins, or can react to [[added]] and [[removed]] if they need
  * to perform special processing when plugins matching their tag are added to or removed from
  * Scaled due to packages being installed or removed. When a plugin set is no longer needed, it
  * should be [[close]]d.
  */
abstract class PluginSet[T <: AbstractPlugin] (val tag: String) extends AutoCloseable {

  /** A signal emitted when a plugin is added to this set.
    * The plugin will be visible in [[plugins]] at the time this event is emitted. */
  def added :SignalV[T]

  /** A signal emitted when a plugin is removed from this set.
    * The plugin will no longer be visible in [[plugins]] at the time this event is emited. The
    * plugin will be [[AbstractPlugin.close]]d after this signal emission has completed. */
  def removed :SignalV[T]

  /** Returns the current plugins in this set. */
  def plugins :SeqV[T]

  /** Closes this plugin set and all the plugins therein. */
  override def close () :Unit = plugins.foreach(_.close())
}

/** An injectable service that allows another service to make use of plugins. */
@Service(name="plugin", impl="impl.PluginManager", desc="Provides access to service plugins.")
trait PluginService {

  /** Resolves plugins tagged with `tag`. Note that the caller is expected to force the
    * type parameter to the type that matches the type implemented by plugins tagged with `tag`.
    * This is not enforceable and you get a class cast exception if you screw up.
    *
    * @param args an optional list of arguments to supply when injecting instances of the requested
    * plugins.
    */
  def resolvePlugins[T <: AbstractPlugin] (tag :String, args :List[Any] = Nil) :PluginSet[T]
}
