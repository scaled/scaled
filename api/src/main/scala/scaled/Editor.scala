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

  /** The workspace in which this editor is opened. */
  def workspace :Workspace

  /** Provides editor-wide config data. Keys are defined in [[EditorConfig]]. */
  def config :Config

  /** Returns all open buffers. The buffers will be returned in order of most recent activation. */
  def buffers :Seq[Buffer]

  /** Provides access to the overlay popup minibuffer. Prefer this for most interactions. */
  def mini :Minibuffer

  /** Provides access to the status-line minibuffer. Use this only when the minibuffer interaction
    * requires the user to see the contents of the main buffer, and hence the popup minibuffer would
    * potentially obscure important data. */
  def statusMini :Minibuffer

  /** Closes this editor. If this is the only open editor, the Scaled process will exit. */
  def exit () :Unit

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** Reports an unexpected error to the user.
    * The message will also be appended to an editor-wide messages list. */
  def emitError (err :Throwable) :Unit

  /** Clears any lingering status message. A status message usually remains visible until the user
    * types the next key, so this allows any buffer which receives key input to clear the last
    * status message. */
  def clearStatus () :Unit

  /** Briefly displays a status message to the user in a popup.
    * The status message will also be appeneded to an editor-wide messages list. */
  def popStatus (msg :String, subtext :String = "") :Unit

  /** Briefly displays a status message to the user.
    * @param ephemeral if false, the status message will also be appended to an editor-wide
    * messages list; if true, it disappears forever in a poof of quantum decoherence. */
  def emitStatus (msg :String, ephemeral :Boolean = false) :Unit

  /** Returns the top-most config scope for `buffer`. */
  def configScope (buffer :Buffer) :Config.Scope

  /** Opens `file` into a new buffer. If another buffer is already visiting `file` that buffer is
    * made active instead.
    * @return the view for the visiting buffer. */
  def visitFile (file :Store) :BufferView

  /** Opens the config file for `mode` into a new buffer. If another buffer is already visiting
    * said file, that buffer is made active instead.
    * @param scope which scope at which to edit the configuration. Valid scopes are enumerated by
    * [[configScopes]].
    * @return the view for the visiting buffer. */
  def visitConfig (scope :Config.Scope, mode :String) :BufferView

  /** Makes the specified buffer the active buffer.
    * @throws IllegalArgumentException if this buffer is not associated with any known buffer view.
    * @return the view for the buffer. */
  def visitBuffer (buffer :Buffer) :BufferView

  /** Used to create new buffers. Obtain via [[Editor.buffer]], then call [[create]. */
  case class BufferConfig (name :String, _reuse :Boolean, _mode :Option[String],
                           _args :List[Any], _state :List[State.Init[_]], _tags :List[String]) {

    /** Requests reuse of an existing buffer with the same name. If a buffer named `buffer` exists,
      * it will be returned directly (as is, so be careful you're not getting an unexpected buffer
      * in this case). Otherwise in the event of name collision, a fresh buffer name will be
      * generated from `buffer` by appending <N> to the name with increasing values of N until an
      * unused name is obtained.
      */
    def reuse () :BufferConfig = copy(_reuse=true)

    /** Configures the the desired major mode. By default the mode is inferred.
      * @param mode the name of the major mode to use.
      * @param args arguments to pass to the major mode during constructor injection.
      */
    def mode (name :String, args :Any*) = copy(_mode=Some(name), _args=List.copyOf(args))

    /** Configures initial buffer state. */
    def state (state :State.Init[_]*) = copy(_state=List.copyOf(state))

    /** Configures additional tags to be used when resolving minor modes for this buffer. These are
      * combined with the tags provided by the major mode. */
    def tags (tags :String*) = copy(_tags=List.copyOf(tags))

    /** Creates the buffer using the current configuration. */
    def create () :BufferView = createBuffer(this)
  }

  /** Returns a buffer config for a buffer named `buffer`. */
  def bufferConfig (buffer :String) :BufferConfig =
    new BufferConfig(buffer, false, None, Nil, Nil, Nil)

  /** Creates a buffer per the specifications of `config`. */
  def createBuffer (config :BufferConfig) :BufferView

  /** Requests to kill the buffer with the specified name. The buffer may not actually be killed due
    * to buffer kill hooks which can abort the kill. */
  def killBuffer (buffer :Buffer) :Unit
}

/** [[Editor]]-related stuffs. */
object Editor {
  import EditorConfig._

  /** Global editor state indicating which virtual desktop on which Scaled is running. */
  case class Desktop (id :String)

  /** The ring in which killed blocks of text are stored. */
  def killRing (editor :Editor) = Mutable.getOrPut(
    rings(editor), "kill", new KillRing(editor.config(killRingSize)))

  /** The history ring for file names (find-file, write-file, etc.). */
  def fileHistory (editor :Editor) = historyRing(editor, "file")
  /** The history ring for buffer names (switch-to-buffer, kill-buffer, etc.). */
  def bufferHistory (editor :Editor) = historyRing(editor, "buffer")
  /** The history ring for replace fns (replace-string, replace-regexp, query-replace, etc.). */
  def replaceHistory (editor :Editor) = historyRing(editor, "replace")
  /** The history ring used for mode names. */
  def modeHistory (editor :Editor) = historyRing(editor, "mode")
  /** The history ring used for fns. */
  def fnHistory (editor :Editor) = historyRing(editor, "fn")
  /** The history ring used for config var names. */
  def varHistory (editor :Editor) = historyRing(editor, "var")
  /** The history ring used for config var values. */
  def setVarHistory (editor :Editor) = historyRing(editor, "set-var")

  /** Returns the (editor-wide) history ring with the specified name. The ring will be created
    * on-demand. Note the history ring names above, which are used by Scaled. */
  def historyRing (editor :Editor, name :String) = Mutable.getOrPut(
    rings(editor), name, new Ring(editor.config(historySize)) {
      override def toString = s"$name-history"
    })

  private def rings (editor :Editor) = editor.state[Rings].getOrElseUpdate(new Rings())
  private class Rings extends HashMap[String,Ring]()
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
