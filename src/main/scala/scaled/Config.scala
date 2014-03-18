//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Helper classes for managing editor configuration. */
object Config {

  /** A key identifies a single configuration setting.
    * @param A human readable description. */
  abstract class Key[T] (val descrip :String) {
    /** The value to use if this setting is not customized by the user. */
    def defval (config :Config) :T
  }

  /** Creates a config key described by `descrip` with default value `default`. */
  def key[T] (descrip :String, default :T) :Key[T] = new Key[T](descrip) {
    override def defval (config :Config) = default
  }

  /** Creates a config key described by `descrip` that defaults to the value of `default`. */
  def key[T] (descrip :String, default :Key[T]) = new Key[T](descrip) {
    override def defval (config :Config) = config(default)
  }

  //
  // Basic Face configuration keys

  val boldFace      = key("Basic bold face.", Face(bold=true))
  val italicFace    = key("Basic italic face.", Face(italic=true))
  val underlineFace = key("Basic underline face.", Face(underline=true))
  val strikeFace    = key("Basic strikethrough face.", Face(strike=true))

  val regionFace = key("Basic face for highlighting the region.", Face("#000044"))

  val warnFace  = key("Basic face used to highlight warnings.", Face("DarkOrange").copy(bold=true))
  val errorFace = key("Basic face used to highlight errors.", Face("Red").copy(bold=true))
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
