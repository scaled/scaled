//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.FileNotFoundException
import reactual.Value
import scaled.util.{Behavior, Close}

/** Provides a mode with a bunch of standard dependencies. We package these up for two reasons:
  *  - one it makes passing a bundle of standard depends on to a superclass constructor less
  *    onerous,
  *  - and two it allows us to provide new standard dependencies without breaking the constructor
  *    signatures of every mode in the known universe.
  */
abstract class Env {

  /** For debug logging. */
  val log :Logger

  /** For executing operations on the UI thread and background threads. */
  val exec :Executor

  /** The editor in which this mode is operating. */
  val editor :Editor

  /** The view in which this mode is operating. */
  val view :RBufferView

  /** The dispatcher that is handling interactions. */
  val disp :Dispatcher

  /** Displays mode information to the user. Extensible. */
  val mline :ModeLine

  /** Resolves the config for `mode` using the supplied `defs`. This is an implementation detail
    * that unfortunately has to live out in the open. Please to ignore. */
  def resolveConfig (mode :String, defs :List[Config.Defs]) :Config
}

/** Defines the attributes of an editor mode (major or minor). An editing mode has two main
  * components:
  *  - a collection of fns; an fn is a function that can be called interactively (by virtue of
  *    being bound to a key, or called by name by the user) and which has its inputs automatically
  *    provided by the environment (see below)
  *  - a key map: a collection of mappings from trigger sequence to fn; these define the "user
  *    interface" of the mode, in that the user will generally interact with a mode by pressing
  *    keys
  *
  * Fns: a fn is a zero-argument method in the `Mode` class which is annotated with [[Fn]]. The
  * name of the method defines the name of the method as referenced in key maps and as specified
  * directly by the user when calling fns by name. The name is de-camel-cased by inserting a dash
  * before every capital letter and making said capital letter lowercase. For example: forwardChar
  * becomes forward-char.
  *
  * Modes are resolved by name and have their "dependencies" (constructor arguments) injected based
  * on their type. Most of these dependencies are wrapped into an [[Env]] instance, but special
  * modes (like `MinibufferMode`s) can route additional dependencies through this injection
  * process. Also minor modes can have the current `MajorMode` injected.
  *
  * A mode must also have a [[Major]] or [[Minor]] annotation, which defines the name of the mode
  * and provides a basic description.
  */
abstract class Mode (env :Env) {

  /** Returns the name of this mode. */
  def name :String

  /** Returns a brief description of this mode. */
  def desc :String

  /** Returns the tags that describe this mode. See [[Major.tags]] and [[Minor.tags]]. */
  def tags :Array[String]

  /** This mode's configuration. */
  final val config :Config = env.resolveConfig(name, configDefs)

  /** Returns the configuration definitions objects that are used by this mode. If a mode defines
    * configurables in a configuration definitions object, it should override this method and
    * prepend its object to the returned list. */
  def configDefs :List[Config.Defs] = Nil

  /** Returns bindings for all of this mode's vars. */
  def varBindings :Seq[Config.VarBind[_]] = configDefs.flatMap(_.vars).map(
    v => Config.VarBind(this, v))

  /** Returns the URL for any custom stylesheets associated with this mode. These should be bundled
    * with the mode and should be referenced via the classloader. A helper method [[stylesheetURL]]
    * is provided to take care of this for you. For example:
    *
    * `override def stylesheets = stylesheetURL("/mymode.css") :: super.stylesheets`
    *
    * The stylesheets will be added in reverse order, so if a mode inherits from a mode that
    * defines a stylesheet, the parent mode's stylesheet will be addede before the subclass's
    * stylesheet, ensuring the subclass's styles override the parent's styles.
    */
  def stylesheets :List[String] = Nil

  /** Returns the key bindings defined by this mode: a list of `(trigger sequence -> fn binding)`
    * mappings.
    *
    * Trigger sequences are defined thusly: A single key consists of the key identifier (e.g. 'g',
    * 'F1', '-',) prefixed by zero or more modifier keys ('C-' for control, 'M-' for meta, 'A-' for
    * alt, and 'S-' for shift). Key sequences consist of single keys separated by spaces. Examples:
    *  - `e`: lowercase e
    *  - `S-s`: upper case S
    *  - `C-c`: control-c
    *  - `C-c C-i`: control-c followed by control-i
    *
    * The fn bindings are defined by the mode, by using the [[Fn]] annotation on methods. The name
    * in the keymap corresponds to the de-camel-cased method name (see [[Mode]] docs). When a mode
    * refers to its own fns, it may provide just the name, but if a mode (or a mode hook) refers to
    * another mode's fns, it must prefix the name by the name of the mode and a colon (e.g.
    * "scala:goto-term").
    *
    * Key bindings are applied in a stack-like fashion:
    *  - start with global key bindings
    *  - push the major mode key bindings
    *  - push customizations to that mode's key bindings specified by the user
    *  - push minor mode key bindings
    *  - push customizations to that mode's key bindings specified by the user
    *  - and so forth for all active minor modes
    *  - finally push global key binding customizations specified by the user.
    *
    * When a key is pressed, the bindings on the top of the stack are searched first (user defined
    * global key bindings), then on down the stack until a match is found, or we fall off the
    * bottom after searching the stock global key bindings.
    */
  def keymap :Seq[(String, String)]

  /** Called when a mode is deactivated but the buffer to which it was attached will remain active.
    * This will precede the call to [[dispose]]. If a mode is maintaining any transient state in the
    * buffer, it should remove it here. That way the cost of removal is avoided if the buffer itself
    * is going to be thrown away too.
    */
  def deactivate () {
    // deactivate any active behaviors now; this ensures that they're informed that the buffer is
    // not going away and they need to do extra work to leave it unmolested
    _toClose foreach {
      case bh :Behavior => bh.setActive(false)
      case _            => // noop!
    }
  }

  /** Cleans up any external resources managed by this mode. This is called when the mode is
    * disabled or the buffer containing the mode is going away. */
  def dispose () {
    try _toClose.close()
    catch {
      case e :Exception => editor.emitError(e)
    }
  }

  // a view methods to make life easier for modes
  @inline protected final def editor = env.editor
  @inline protected final def view = env.view
  @inline protected final def buffer = env.view.buffer
  @inline protected final def disp = env.disp

  /** Binds `behavior` to the specified boolean configuration key. When `key` is true, the behavior
    * will be activated, when it is false it will be deactivated.
    */
  protected def addBehavior (key :Config.Key[Boolean], behavior :Behavior) {
    // bind behavior's activation to the specified config key
    note(config.value(key) onValueNotify behavior.setActive)
    note(behavior) // deactivate the behavior (if active) when we're disposed
  }

  /** Adds `close` to a list to be closed when this mode is disposed. This is useful for noting
    * Reactual connections that must be closed when a mode is disconnected, or [[Behavior]]s that
    * must be deactivated when a mode is disposed, etc.
    */
  protected def note (close :AutoCloseable) :Unit = _toClose += close

  /** Removes `close` from the to-close-on-dispose list. This is unnecessary if the closeable is
    * idempotent as the extra `close()` call will be ignored. But if repeated `close()` causes
    * trouble, then this enables manual management.
    */
  protected def unnote (close :AutoCloseable) :Unit = _toClose -= close

  /** A helper function for obtaining a stylesheet URL from a classpath. A mode will generally call
    * this like so: `stylesheetURL("/mymode.css")` and place `mymode.css` in the top-level of the
    * mode's resources directory. */
  protected def stylesheetURL (path :String) = getClass.getResource(path) match {
    case null => throw new FileNotFoundException(s"Unable to find stylesheet resource '$path'")
    case rsrc => rsrc.toExternalForm
  }

  private[this] val _toClose = Close.bag()
}

/** Provides the foundation for a major editing mode. A major editing mode customizes the behavior
  * of the editor, usually while editing a certain type of file (a `.java` source file, for
  * example).
  *
  * The mode instance generally exists for the lifetime of a buffer, and can wire up reactions to
  * changes in the buffer or editor in addition to making simpler behavior changes like modifying
  * the keymap.
  */
abstract class MajorMode (env :Env) extends Mode(env) {

  override def name = if (info == null) "unknown" else info.name
  override def desc = if (info == null) "unknown" else info.desc
  override def tags = if (info == null) Array()   else info.tags
  private lazy val info = getClass.getAnnotation(classOf[Major])

  // display our major mode name in the modeline
  env.mline.addDatum(Value(s"[$name]"), "Active major mode")

  /** The default fn to invoke for a key press for which no mapping exists. This will only be called
    * for key presses that result in a "typed" character. Key presses that do not generate
    * characters (i.e. F1, HOME) or which are modified by modifiers other than SHIFT will be passed
    * to [[missedFn]]. */
  def defaultFn :Option[String] = None

  /** The fn to invoke for a key press for which no mapping exists and which is not expected to yield
    * text which can be inserted into the buffer (such key presses are sent to [[defaultFn]]).
    * Instead of supplying `typed` text to the fn, the string representation of the trigger
    * sequence will be supplied instead (e.g. `C-x C-p`). */
  def missedFn :Option[String] = None
}

/** Provides the foundation for a minor editing mode. A minor editing mode customizes the behavior
  * of the editor in a way that augments the behavior of one or more major editing modes (for
  * example, by checking the spelling of all words in the buffer and binding a face to those that
  * are misspelled).
  */
abstract class MinorMode (env :Env) extends Mode(env) {

  override def name = if (info == null) "unknown" else info.name
  override def desc = if (info == null) "unknown" else info.desc
  override def tags = if (info == null) Array()   else info.tags
  private lazy val info = getClass.getAnnotation(classOf[Minor])
}
