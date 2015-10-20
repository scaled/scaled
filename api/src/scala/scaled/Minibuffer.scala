//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Invokes the minibuffer with specific arguments. Also provides some convenience methods. */
trait Minibuffer {

  /** Invokes the minibuffer with mode `mode`, supplying `result` and `args` as arguments to the
    * mode. These arguments are used to resolve the mode's constructor dependencies. Note that
    * `mode` will be prefixed with the string `mini-` as all minibuffer modes are required to
    * follow that naming convention. Thus `"read"` will resolve a mode named `"mini-read"`.
    *
    * A `Promise` of the appropriate type will be created and prepended to `args` via which the
    * mode will communicate its results. That promise is returned (as a `Future`) via which the
    * caller can receive a result from the minibuffer mode. For example:
    *
    * {{{
    * editor.mini("read", Promise[String](), "Find file: ", ...) onSuccess { path => ... }
    * }}}
    */
  def apply[R] (mode :String, args :Any*) :Future[R]

  /** Prompts the user to input a string via the minibuffer. Returns a future which will yield the
    * entered string, or which will fail if input was canceled.
    *
    * @param prompt the text to display to the left of the minibuffer when requesting input.
    * @param defval the default value with which to populate the minibuffer.
    * @param history a ring that's used for history when the user presses UP/DOWN.
    * @param completer used to generate completions when the user presses TAB.
    */
  def read[R] (prompt :String, defval :String, history :Ring, completer :Completer[R]) :Future[R] =
    apply("read", prompt, Line.fromText(defval), history, completer)

  /** Prompts the user to enter 'y' or 'n' via the minibuffer. Returns a future which will yield
    * true for 'y', false for 'n', and which will fail if input was canceled.
    *
    * @param prompt the text to display when requesting input. The string ` (y or n)` will be
    * automatically appended.
    */
  def readYN (prompt :String) :Future[Boolean] = apply("yesno", prompt)
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
    apply("readopt", prompt, opts)
}
