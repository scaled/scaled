//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Promise}

/** Provides access to data and services encapsulated by the editor. The editor is not quite the
  * entire app, but rather a single window which contains a set of buffers and state geared toward
  * working on one "thing". A user may wish to do everything in one editor, or they may have
  * multiple editors open (perhaps one on each virtual workspace), each dedicated to a distinct
  * task. As such, buffers are not shared between editors, but background services are, because
  * those can be shared without impacting the user experience.
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

  /** Invokes the minibuffer with mode `mode`, supplying `result` and `args` as arguments to the
    * mode. These arguments are used to resolve the mode's constructor dependencies. Note that
    * `mode` will be prefixed with the string `mini-` as all minibuffer modes are required to
    * follow that naming convention. Thus `"read"` will resolve a mode named `"mini-read"`.
    *
    * The first mode argument (`result`) is returned back to the caller with the expectation that
    * it will be a `Promise` via which the mode will communicate its results. This allows
    * minibuffer calls which deliver results to proceed fluently:
    * {{{
    * editor.mini("read", Promise[String](), "Find file: ", ...) onSuccess { path => ... }
    * }}}
    *
    * See [[Mode]] and [[major.MinibufferMode]] for more information on how `result` and `args` are
    * used in the minibuffer mode's dependency resolution process.
    */
  def mini[R] (mode :String, result :Promise[R], args :Any*) :Future[R]

  /** Prompts the user to input a string via the minibuffer. Returns a future which will yield the
    * entered string, or which will fail if input was canceled.
    *
    * @param prompt the text to display to the left of the minibuffer when requesting input.
    * @param defval the default value with which to populate the minibuffer.
    * @param history a ring that's used for history when the user presses UP/DOWN.
    * @param completer used to generate completions when the user presses TAB.
    */
  def miniRead[R] (prompt :String, defval :String, history :Ring,
                   completer :Completer[R]) :Future[R] =
    mini("read", Promise[R](), prompt, Line.fromText(defval), history, completer)

  /** Prompts the user to enter 'y' or 'n' via the minibuffer. Returns a future which will yield
    * true for 'y', false for 'n', and which will fail if input was canceled.
    *
    * @param prompt the text to display when requesting input. The string ` (y or n)` will be
    * automatically appended.
    */
  def miniReadYN (prompt :String) :Future[Boolean] = mini("yesno", Promise[Boolean](), prompt)
  // TODO: should I just return Future[Unit] and automatically emit "Canceled." and fail the future
  // if they choose 'n'? that would make chaining confirmations simpler/more succinct...

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
}
