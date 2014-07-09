//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Promise, OptValue}

/** Provides access to data and services encapsulated by the editor. The editor is not quite the
  * entire app, but rather a single window which contains a set of buffers and state geared toward
  * working on one "thing". A user may wish to do everything in one editor, or they may have
  * multiple editors open (perhaps one on each virtual desktop), each dedicated to a distinct task.
  * As such, buffers are not shared between editors, but background services are, because those can
  * be shared without impacting the user experience.
  */
trait Editor {

  /** Closes this editor. If this is the only open editor, the Scaled process will exit. */
  def exit () :Unit

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** Briefly displays a status message to the user in a popup.
    * The status message will also be appeneded to an editor-wide messages list. */
  def popStatus (msg :String, subtext :String = "") :Unit

  /** Briefly displays a status message to the user.
    * @param ephemeral if false, the status message will also be appended to an editor-wide
    * messages list; if true, it disappears forever in a poof of quantum decoherence. */
  def emitStatus (msg :String, ephemeral :Boolean = false) :Unit

  /** Reports an unexpected error to the user.
    * The message will also be appended to an editor-wide messages list. */
  def emitError (err :Throwable) :Unit

  /** Clears any lingering status message. A status message usually remains visible until the user
    * types the next key, so this allows any buffer which receives key input to clear the last
    * status message. */
  def clearStatus () :Unit

  /** Provides access to the overlay popup minibuffer. Prefer this for most interactions. */
  def mini :Minibuffer

  /** Provides access to the status-line minibuffer. Use this only when the minibuffer interaction
    * requires the user to see the contents of the main buffer, and hence the popup minibuffer would
    * potentially obscure important data. */
  def statusMini :Minibuffer

  /** A reactive mapping of editor-wide state. */
  def state :State

  /** Returns all open buffers. The buffers will be returned in order of most recent activation. */
  def buffers :Seq[Buffer]

  /** Opens `file` into a new buffer. If another buffer is already visiting `file` that buffer is
    * made active instead.
    * @return the view for the visiting buffer. */
  def visitFile (file :Store) :BufferView

  /** Opens the config file for `mode` into a new buffer. If another buffer is already visiting said
    * file, that buffer is made active instead.
    * @return the view for the visiting buffer. */
  def visitConfig (mode :String) :BufferView

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
    def mode (name :String, args :Any*) = copy(_mode=Some(name), _args=args.toList)

    /** Configures initial buffer state. */
    def state (state :State.Init[_]*) = copy(_state=state.toList)

    /** Configures additional tags to be used when resolving minor modes for this buffer. These are
      * combined with the tags provided by the major mode. */
    def tags (tags :String*) = copy(_tags=tags.toList)

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
