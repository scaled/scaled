//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

import reactual.{Future, Promise}

/** Provides access to certain global functionality that doesn't fit nicely elsewhere. */
trait Editor {

  /** Terminates the editor.
    * @param code the status code to report to the operating system.
    */
  def exit (code :Int) :Unit

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** Invokes `op` on the next UI tick. */
  def defer (op : =>Unit) :Unit

  /** The kill ring shared by all buffers in this editor. */
  def killRing :KillRing

  /** Briefly displays a status message in the minibuffer. The status message will also be appeneded
    * to an editor-wide messages list. */
  def emitStatus (msg :String) :Unit

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
    * See [[Mode]] and [[MinibufferMode]] for more information on how `result` and `args` are used
    * in the minibuffer mode's dependency resolution process.
    */
  def mini[R] (mode :String, result :Promise[R], args :Any*) :Future[R]

  /** Prompts the user to input a string via the minibuffer. Returns a future which will yield the
    * entered string, or which will fail if input was canceled.
    *
    * @param prompt the text to display to the left of the minibuffer when requesting input.
    * @param defval the default value with which to populate the minibuffer.
    * @param completer a completion function used to generate completions when the user presses TAB.
    *                  It will be passed the current minibuffer contents (the prefix) and should
    *                  return the set of all valid completions. The completions should be the
    *                  entire values, not suffixes to be appended to the prefix.
    */
  def miniRead (prompt :String, defval :String, completer :String => Set[String]) :Future[String] =
    mini("read", Promise[String](), prompt, Line.fromText(defval), completer)

  /** Prompts the user to enter 'y' or 'n' via the minibuffer. Returns a future which will yield true
    * for 'y', false for 'n', and which will fail if input was canceled.
    *
    * @param prompt the text to display when requesting input. The string ` (y or n)` will be
    * automatically appended.
    */
  def miniReadYN (prompt :String) :Future[Boolean] = mini("yesno", Promise[Boolean](), prompt)
  // TODO: should I just return Future[Unit] and automatically emit "Canceled." and fail the future
  // if they choose 'n'? that would make chaining confirmations simpler/more succinct...

  /** Returns a view on all open buffers. The buffers will be returned in order of most recent
    * activation. */
  def buffers :Seq[BufferV]

  /** Opens `file` into a new buffer. If another buffer is already visiting `file` that buffer is
    * made active instead. */
  def newBuffer (file :File) :Unit

  /** Makes the buffer with the specified name the active buffer. If no buffer exists with that
    * name, a new empty buffer will be created with that name.. */
  def openBuffer (buffer :String) :Unit

  /** Requests to kill the buffer with the specified name. The buffer may not actually be killed due
    * to buffer kill hooks which can abort the kill.
    * @return true if `buffer` exists and the request was initiated, false if no such buffer
    * exists. */
  def killBuffer (buffer :String) :Boolean
}
