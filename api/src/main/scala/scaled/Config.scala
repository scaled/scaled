//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.{FileNotFoundException, InputStream}
import java.nio.file.{Files, Paths}
import scaled.util.Reloadable

/** Provides editor configuration to modes. The global configuration and mode configuration will be
  * combined together into an instance of this trait and supplied to a mode when it is constructed.
  *
  * A configuration consists of a collection of settings, which can be customized by the user. Each
  * setting is identified by a [ConfigKey] and maps to some value. The value may be of any type,
  * usually that's a simple value type (Int, String, etc.), but it could also be a function that's
  * meant to be configurable, or editor- or mode-wide state (like a [[Ring]]).
  *
  * Configuration entries are reactive values and modes are encouraged to react to changes in
  * configuration entries which are designed to be customized by the end user.
  */
trait Config {

  /** Returns a reactive view of the value of `key`. */
  def value[T] (key :Config.Key[T]) :ValueV[T]

  /** Resolves the currently configured value for `key`. */
  def apply[T] (key :Config.Key[T]) :T = value(key).get

  /** Updates the value for `key` in the current buffer only. */
  def update[T] (key :Config.Key[T], value :T) :Unit
}

object Config {

  /** Converts `name` from camelCase to words-separated-by-dashes. */
  def deCamelCase (name :String) = {
    val buf = new StringBuilder(name)
    var ii = 0
    while (ii < buf.length) {
      val c = buf.charAt(ii)
      if (Character.isUpperCase(c)) {
        buf.deleteCharAt(ii)
        buf.insert(ii, '-')
        ii += 1
        buf.insert(ii, Character.toLowerCase(c))
      }
      ii += 1
    }
    buf.toString
  }

  /** A key that identifies a single configuration setting.
    * @param global whether this config is editor global or local to a major or minor mode.
    */
  abstract class Key[T] (val global :Boolean, val converter :Converter[T]) {

    /** The value to use if this setting is not customized by the user. */
    def defval (config :Config) :T

    /** Converts `value` to a string using this key's converter. */
    def show (value :T) = converter.show(value)
    /** Converts `value` from a string using this key's converter. */
    def read (value :String) = converter.read(value)

    override def toString () = (if (global) "global" else "local") + "/" + converter
  }

  /** Converts values to and from strings. */
  abstract class Converter[T] {
    def show (value :T) :String
    def read (value :String) :T
  }
  object IntC extends Converter[Int] {
    override def show (value :Int) = value.toString
    override def read (value :String) = value.toInt
    override def toString = "Int"
  }
  object BoolC extends Converter[Boolean] {
    override def show (value :Boolean) = value.toString
    override def read (value :String) =
      (value equalsIgnoreCase "true") || (value equalsIgnoreCase "t")
    override def toString = "Boolean"
  }
  object StringC extends Converter[String] {
    override def show (value :String) = value
    override def read (value :String) = value
    override def toString = "String"
  }

  /** Contains metadata for a particular configuration var. */
  case class Var[T] (name :String, descrip :String, key :Key[T]) {
    /** Returns the current value of this var in `config`, converted to a string. */
    def current (config :Config) :String = key.show(config(key))
    /** Converts `value` to the appropriate type for this var and updates it in `config`. */
    def update (config :Config, value :String) = config(key) = key.read(value)
  }

  /** Eases the process of working with a mode's var bindings. */
  case class VarBind[T] (m :Mode, v :Var[T]) {
    /** Returns the current value of this var binding, converted to a string. */
    def current :String = v.current(m.config)
    /** Converts `value` to the appropriate type for this var binding and updates it. */
    def update (value :String) = v.update(m.config, value)
  }

  /** The base class for a collection of config definitions. The (global) editor config object
    * ([[EditorConfig]]) extends this as well as each individual mode's config definition object.
    */
  abstract class Defs (global :Boolean = false) {

    /** All config vars defined by this defs instance. */
    lazy val vars :Seq[Var[_]] = getClass.getDeclaredFields.mkSeq foldBuild[Var[_]] { (b, f) =>
      val vara = f.getAnnotation(classOf[scaled.Var])
      if (vara != null) {
        f.setAccessible(true)
        val key = f.get(this).asInstanceOf[Key[Object]]
        b += new Var[Object](deCamelCase(f.getName), vara.value.replaceAll("\\n\\s+", " "), key)
      }
    }

    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :Boolean) = new Config.Key[Boolean](global, BoolC) {
        override def defval (config :Config) = default
      }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :Int) = new Config.Key[Int](global, IntC) {
        override def defval (config :Config) = default
      }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :String) = new Config.Key[String](global, StringC) {
      override def defval (config :Config) = default
    }
    // TODO: other primitive types? lists? sets? maps?

    /** Creates a config key described by `desc` that defaults to the value of `default`. */
    protected def key[T] (default :Config.Key[T]) = new Config.Key[T](global, default.converter) {
      override def defval (config :Config) = config(default)
    }

    /** Creates a config key described by `desc` that defaults to the value generated by applying
      * `deffn` to the current configuration. `deffn` will only be called once to generate the value
      * and that value will be used for the lifetime of the configuration instance. */
    protected def fnKey[T] (deffn :Config => T) = new Config.Key[T](global, null) {
      override def defval (config :Config) = deffn(config)
    }

    // useful for loading resources in our configs
    protected def stream (path :String) = getClass.getClassLoader.getResourceAsStream(path) match {
      case null => throw new FileNotFoundException(path)
      case strm => strm
    }

    protected def reloadable[T] (path :String, parser :InputStream => T) :PropertyV[T] = {
      val rsrc = getClass.getClassLoader.getResource(path)
      if (rsrc == null) throw new FileNotFoundException(path)
      // if the resource is not a file for some reason, we can't reload it
      else if (rsrc.getProtocol != "file") new PropertyV[T] {
        val v = parser(rsrc.openStream)
        override def get = v
        override def apply () = v
      }
      else new Reloadable(Paths.get(rsrc.toURI), parser.compose(Files.newInputStream(_)))
    }
  }
}

/** Defines editor-global configurables. */
object EditorConfig extends Config.Defs(true) {

  @Var("""The default x position of editor views, in pixels.
          -1 indicates that the view should be centered in the screen.""")
  val viewLeft = key(-1)
  @Var("""The default y position of editor views, in pixels.
          -1 indicates that the view should be centered in the screen.""")
  val viewTop = key(-1)

  @Var("The default width of editor views, in characters.")
  val viewWidth = key(100)
  @Var("The default height of editor views, in characters.")
  val viewHeight = key(40)

  @Var("The number of entries retained by the kill ring.")
  val killRingSize = key(40)

  @Var("The number of entries retained in most minibuffer histories.")
  val historySize = key(40)

  /** The ring in which killed blocks of text are stored. */
  val killRing = fnKey(cfg => new KillRing(cfg(killRingSize)))

  /** The history ring for file names (find-file, write-file, etc.). */
  val fileHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring for buffer names (switch-to-buffer, kill-buffer, etc.). */
  val bufferHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring for replace fns (replace-string, replace-regexp, query-replace, etc.). */
  val replaceHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring used for mode names. */
  val modeHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring used for fns. */
  val fnHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring used for config var names. */
  val varHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The history ring used for config var values. */
  val setVarHistory = fnKey(cfg => new Ring(cfg(historySize)))

  /** The default CSS class name for text. */
  val textStyle = "textFace"
  /** The CSS class name for the active region face. */
  val regionStyle = "regionFace"
  /** The CSS class name for `warn` face. */
  val warnStyle = "warnFace"
  /** The CSS class name for `error` face. */
  val errorStyle = "errorFace"

  /** The CSS class name for (non-semantic) bold text. */
  val boldStyle = "boldFace"
  /** The CSS class name for (non-semantic) italicized text. */
  val italicStyle = "italicFace"
  /** The CSS class name for (non-semantic) underlined text. */
  val underlineStyle = "underlineFace"
  /** The CSS class name for (non-semantic) struck-through text. */
  val strikeStyle = "strikeFace"

  /** The CSS style applied to search matches. */
  val matchStyle = "matchFace"
  /** The CSS style applied to the active search match. */
  val activeMatchStyle = "activeMatchFace"
}
