//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Promise}

/** Invokes the minibuffer with specific arguments. Also provides some convenience methods. */
trait Minibuffer {

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
  def apply[R] (mode :String, result :Promise[R], args :Any*) :Future[R]

  /** Prompts the user to input a string via the minibuffer. Returns a future which will yield the
    * entered string, or which will fail if input was canceled.
    *
    * @param prompt the text to display to the left of the minibuffer when requesting input.
    * @param defval the default value with which to populate the minibuffer.
    * @param history a ring that's used for history when the user presses UP/DOWN.
    * @param completer used to generate completions when the user presses TAB.
    */
  def read[R] (prompt :String, defval :String, history :Ring, completer :Completer[R]) :Future[R] =
    apply("read", Promise[R](), prompt, Line.fromText(defval), history, completer)

  /** Prompts the user to enter 'y' or 'n' via the minibuffer. Returns a future which will yield
    * true for 'y', false for 'n', and which will fail if input was canceled.
    *
    * @param prompt the text to display when requesting input. The string ` (y or n)` will be
    * automatically appended.
    */
  def readYN (prompt :String) :Future[Boolean] = apply("yesno", Promise[Boolean](), prompt)
  // TODO: should I just return Future[Unit] and automatically emit "Canceled." and fail the future
  // if they choose 'n'? that would make chaining confirmations simpler/more succinct...

  /** Prompts the user to enter one of the supplied `opts`. Returns a future which contains the
    * chosen option. An additional option, `C-h`, will be automatically added which displays the
    * options and their accompanying help strings.
    *
    * @param prompt the text to display when requesting input.
    * @param opts a sequence of `(key trigger, help text)` pairs which define the options.
    */
  def readOpt (prompt :String, opts :Seq[(String,String)]) :Future[String] =
    apply("readopt", Promise[String](), prompt, opts)
}
