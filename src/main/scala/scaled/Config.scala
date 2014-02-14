//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Helper classes for managing editor configuration. */
object Config {

  /** A key identifies a single configuration setting. */
  case class Key[T] (
    /** A human readable description. */
    descrip :String,
    /** The value to use if this setting is not customized by the user. */
    defval :T)

  /** Creates a configuration key. */
  def key[T] (descrip :String, defval :T) = Key(descrip, defval)

  /** The key used to configure global and per-mode key bindings. */
  val keymap = key("Key bindings.", Seq[KeyBinding]())
}

/** Manages editor configuration. This trait is used to manage the global configuration for the
  * editor, as well as a configuration for each major and minor mode.
  *
  * A configuration consists of a collection of settings, which can be customized by the user. Each
  * setting is identified by a [Config.Key] and maps to some value. The value may be of a simple
  * type (Int, String, etc.) or it may be a function that is meant to be configurable.
  */
trait Config {

  /** Resolves the currently configured value for `key`. */
  def apply[T] (key :Config.Key[T]) :T
}
