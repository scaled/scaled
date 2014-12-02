//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.{FileNotFoundException, InputStream}
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import scaled.util.Resource

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

  /** The scope at which this instance was resolved. */
  def scope :Config.Scope

  /** Returns the file that backs this config object. */
  def file :Path

  /** Converts this config to a `properties` file. Config vars that have not been changed from
    * their default values will have commented out entries indicating the default values, and
    * configured entries will appear as normal.
    */
  def toProperties :Seq[String]

  /** Returns `this` config if [[scope]] == `scope`, or passes the buck up our parent chain.
    * @throws IllegalArgumentException if no parent matches the specified scope.
    */
  def atScope (scope :Config.Scope) :Config
}

object Config {

  /** A key that identifies a single configuration setting.
    * @param global whether this config is editor global or local to a major or minor mode.
    */
  abstract class Key[T] (val converter :Converter[T]) {

    /** The value to use if this setting is not customized by the user. */
    def defval (config :Config) :T

    /** Converts `value` to a string using this key's converter. */
    def show (value :T) = converter.show(value)
    /** Converts `value` from a string using this key's converter. */
    def read (value :String) = converter.read(value)

    override def toString () = converter.toString
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
  object JBoolC extends Converter[JBoolean] {
    override def show (value :JBoolean) = value.toString
    override def read (value :String) =
      (value equalsIgnoreCase "true") || (value equalsIgnoreCase "t")
    override def toString = "Boolean"
  }
  object JIntC extends Converter[JInteger] {
    override def show (value :JInteger) = value.toString
    override def read (value :String) = value.toInt
    override def toString = "Int"
  }
  object StringC extends Converter[String] {
    override def show (value :String) = value
    override def read (value :String) = value
    override def toString = "String"
  }
  object StringsC extends Converter[Seq[String]] {
    override def show (value :Seq[String]) = value.mkString(", ")
    override def read (value :String) = Seq.from(value.split(","))
    override def toString = "Strings"
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

  /** The base class for a collection of config definitions. */
  abstract class Defs () {

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
    protected def key (default :Boolean) = new Config.Key(BoolC) {
      override def defval (config :Config) = default
    }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :Int) = new Config.Key(IntC) {
      override def defval (config :Config) = default
    }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :String) = new Config.Key[String](StringC) {
      override def defval (config :Config) = default
    }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :Seq[String]) = new Config.Key[Seq[String]](StringsC) {
      override def defval (config :Config) = default
    }
    // TODO: other types: sets? maps?

    // some specializations for Java interop
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :JBoolean) = new Config.Key(JBoolC) {
      override def defval (config :Config) = default
    }
    /** Creates a config key described by `desc` with default value `default`. */
    protected def key (default :JInteger) = new Config.Key(JIntC) {
      override def defval (config :Config) = default
    }

    /** Creates a config key described by `desc` that defaults to the current value of the
      * dependent config value identified by `default`. */
    protected def key[T] (default :Config.Key[T]) = new Config.Key[T](default.converter) {
      override def defval (config :Config) = config(default)
    }

    // useful for loading resources in our configs
    protected def stream (path :String) = getClass.getClassLoader.getResourceAsStream(path) match {
      case null => throw new FileNotFoundException(path)
      case strm => strm
    }

    protected def resource[T] (path :String)(parser :Resource => T) :PropertyV[T] =
      Resource(rsrcURL(path)).toProperty(parser)
    protected def resource[T] (paths :Seq[String])(parser :Resource => T) :PropertyV[T] =
      Resource(paths map rsrcURL).toProperty(parser)

    private def rsrcURL (path :String) :URL = {
      val url = getClass.getClassLoader.getResource(path)
      if (url == null) throw new FileNotFoundException(path)
      url
    }
  }

  /** Defines the name and root path for a config scope. A buffer obtains configuration information
    * from a stack of config scopes, the top-most scopes on the stack taking precedence over the
    * bottom-most. Scaled defines a `global` scope at the bottom, and a `workspace` scope atop
    * that. Packages can define more fine grained scopes and layer them atop these scopes to allow
    * the user to customize Scaled on, for example, a per-project or per-directory basis.
    *
    * Scope information is spread across four `State` instances: global state, workspace state,
    * editor state, and buffer state. The config system combines these sub-stacks into a single
    * stack by concatenating thusly: `buffer ::: editor ::: workspace ::: global`. To insert a new
    * scope into the stack, do the following with a state instance at the desired level:
    *
    * `state[Scope]() = Scope(name, root, state.get[Scope])`
    *
    * @param name a human readable name for this scope. Scope names must be unique within a given
    * stack, and the names `global` and `workspace` are reserved by Scaled.
    * @param root the root directory for this scope (in which a `Config` sub-directory will be
    * created).
    * @param next the next scope in this stack.
    */
  case class Scope (name :String, root :Path, next :Option[Scope]) {
    /** Converts this scope's implicit stack to a list (with this scope first). Each scope in the
      * list will preserve its sub-stack. This is mainly useful when completing scope names. */
    def toList :List[Scope] = this :: (next.map(_.toList) || Nil)
    override def toString = s"Scope($name, $root) :: " + next.fold("Nil")(_.toString)
    // used by Scope.apply to concatenate scope sub-stacks
    private def concat (t :Option[Scope]) :Scope = copy(next = next.fold(t)(n => Some(n concat t)))
  }
  /** [[Scope]] helpers. */
  object Scope {
    /** Extracts the scopes from the supplied states and concatenates them into a single stack.
      * @throws NoSuchElementException if no scope is defined in any of the supplied states. */
    def apply (states :StateV*) :Scope = {
      val ss = states.map(_.get[Scope]).dropWhile(!_.isDefined)
      (ss.head.get /: ss.tail)(_ concat _)
    }
  }

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
}
