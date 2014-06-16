//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.SignalV

/** Scaled plugins must extend this class so that they can be notified of lifecycle events. */
abstract class AbstractPlugin {

  /** A callback invoked when a plugin is first resolved by Scaled. */
  def wasResolved () {}

  /** A callback invoked when a plugin is about to be unloaded (along with the package that
    * exported it). */
  def willUnload () {}
}

/** Manages the set of plugins that match a particular tag. A service that's making use of plugins
  * will obtain a plugin set for a particular tagged plugin and can then simply access [[plugins]]
  * when they need to enumerate all plugins, or can react to [[added]] and [[removed]] if they need
  * to perform special processing when plugins matching their tag are added to or removed from
  * Scaled due to packages being installed or removed.
  */
abstract class PluginSet[T <: AbstractPlugin] (val tag: String) {

  /** A signal emitted when a plugin is added to this set.
    * The plugin will be visible in [[plugins]] at the time this event is emitted. */
  def added :SignalV[T]

  /** A signal emitted when a plugin is removed from this set.
    * The plugin will no longer be visible in [[plugins]] at the time this event is emited. */
  def removed :SignalV[T]

  /** Returns the current plugins in this set. */
  def plugins :Seq[T]
}

/** An injectable service that allows another service to make use of plugins. */
@Service(name="plugin", impl="impl.PluginManager", desc="Provides access to service plugins.")
trait PluginService {

  /** Resolves plugins tagged with `tag`. Note that the caller is expected to force the
    * type parameter to the type that matches the type implemented by plugins tagged with `tag`.
    * This is not enforceable and you get a class cast exception if you screw up. */
  def resolvePlugins[T <: AbstractPlugin] (tag :String) :PluginSet[T]
}
