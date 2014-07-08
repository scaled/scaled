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

  /** Creates a new buffer with the specified name. NOTE: the buffer will not be visited (made the
    * active buffer). Follow this call with a call to [[visitBuffer]] if that is desired.
    *
    * @param reuse if true and a buffer named `buffer` exists, it will be returned directly
    * (as is, so be careful you're not getting an unexpected buffer in this case). Otherwise
    * in the event of name collision, a fresh buffer name will be generated from `buffer` by
    * appending <N> to the name with increasing values of N until an unused name is obtained.
    * @param mode specifies the desired mode for the buffer and any custom injection arguments.
    * The mode will be auto-detected if `Infer` is supplied.
    * @return the view for the buffer.
    */
  def createBuffer (buffer :String, reuse :Boolean, mode :ModeInfo = ModeInfo.Infer) :BufferView

  /** Requests to kill the buffer with the specified name. The buffer may not actually be killed due
    * to buffer kill hooks which can abort the kill. */
  def killBuffer (buffer :Buffer) :Unit

  /** Returns the state value associated with the specified type, if any.
    * This mechanism is a simple way for modes and services to maintain editor-wide state. */
  def state[T] (klass :Class[T]) :OptValue[T]
}
