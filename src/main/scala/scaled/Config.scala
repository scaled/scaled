//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.ValueV

/** Provides editor configuration to modes. The global configuration and mode configuration will be
  * encapsulated together into an instance of this trait and supplied to a mode when it is
  * constructed.
  *
  * A configuration consists of a collection of settings, which can be customized by the user. Each
  * setting is identified by a [ConfigKey] and maps to some value. The value may be of a simple
  * type (Int, String, etc.) or it may be a function that is meant to be configurable.
  */
trait Config {

  /** Returns a reactive view of the value of `key`. */
  def value[T] (key :ConfigKey[T]) :ValueV[T]

  /** Resolves the currently configured value for `key`. */
  def apply[T] (key :ConfigKey[T]) :T = value(key).get

  /** Updates the value for `key` in the current buffer only. */
  def update[T] (key :ConfigKey[T], value :T) :Unit
}

/** A key identifies a single configuration setting.
  * @param A human readable description. */
abstract class ConfigKey[T] (val descrip :String) {

  /** The value to use if this setting is not customized by the user. */
  def defval (config :Config) :T
}

/** The base class for a collection of configuration definitions. The (global) editor config object
  * ([[EditorConfig]]) extends this as well as each individual mode's config definition object.
  */
abstract class ConfigDefs {

  /** Creates a config key described by `descrip` with default value `default`. */
  protected def key[T] (descrip :String, default :T) :ConfigKey[T] = new ConfigKey[T](descrip) {
    override def defval (config :Config) = default
  }

  /** Creates a config key described by `descrip` that defaults to the value of `default`. */
  protected def key[T] (descrip :String, default :ConfigKey[T]) = new ConfigKey[T](descrip) {
    override def defval (config :Config) = config(default)
  }
}

/** Defines editor-global configurables. */
object EditorConfig extends ConfigDefs {

  val viewWidth = key("The default width of editor views, in characters.", 100)
  val viewHeight = key("The default height of editor views, in characters.", 40)
}
