//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.HashMap

/** Provides access to data and services encapsulated by the editor. The editor is not quite the
  * entire app, but rather a single window which contains a set of buffers and state geared toward
  * working on one "thing". A user may wish to do everything in one editor, or they may have
  * multiple editors open (perhaps one on each virtual desktop), each dedicated to a distinct task.
  * As such, buffers are not shared between editors, but background services are, because those can
  * be shared without impacting the user experience.
  */
trait Editor {

  /** An event bus for editor-wide event dispatch. */
  val events :EventBus = new EventBus()

  /** A reactive mapping of editor-wide state. */
  val state :State = new State()

  /** Provides global config data. Keys are defined in [[EditorConfig]]. */
  def config :Config

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** The ring in which killed blocks of text are stored. */
  def killRing = Mutable.getOrPut(
    Rings(state), "kill", new KillRing(config(EditorConfig.killRingSize)))

  // TODO: def workspaces :SeqV[Workspace]?

  /** A signal emitted when a workspace is opened. */
  def workspaceOpened :SignalV[Workspace]
}

/** Defines global configurables. */
object EditorConfig extends Config.Defs {

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
