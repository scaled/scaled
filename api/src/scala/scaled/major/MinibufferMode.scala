//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._
import scaled.util.Errors

/** Provides some stock UI services to minibuffer modes. This is a special dependency that can be
  * injected by [[MinibufferMode]]s. */
abstract class MiniUI {

  /** Sets the minibuffer prompt to `prompt`. */
  def setPrompt (prompt :String) :Unit

  /** Returns the current minibuffer prompt. */
  def getPrompt :String

  /** Displays the supplied completion strings. */
  def showCompletions (comps :SeqV[String]) :Unit
}

/** The base class for modes which operate in the minibuffer. These modes generally manage the
  * process of obtaining input from the user and reporting it back to the major mode which invoked
  * the minibuffer. However, some minibuffer modes (like [[ISearchMode]]) are substantially more
  * sophisticated.
  *
  * Minibuffer modes are instantiated using the same "dependency injection" mechanism that normal
  * modes are, but with additional dependencies provided by the invoker of the mode. Note that due
  * to the way that mode dependencies are injected, if a minibuffer mode requires a dependency of
  * the same type as a standard dependency (say for example it needs the invoking major mode's
  * `BufferView`) it must receive the normal dependency of that type in its constructor argument
  * list first, and then the additional dependency. For example:
  *
  * {{{
  * class MiniFooMode (..., miniView :RBufferView, ..., majorView :RBufferView)
  *   extends MinibufferMode(..., miniView, ...)
  * }}}
  *
  * The invoker of the mode supplies a `Promise` instance via which to receive the results of the
  * minibuffer's query. The completion (with success or failure) of this promise causes the
  * minibuffer view to be dismissed.
  *
  * Note: minibuffer mode names must start with the string `mini-`. For example: `mini-isearch`,
  * `mini-read`, etc.
  */
abstract class MinibufferMode (env :Env, promise :Promise[_]) extends EditingMode(env) {

  override def keymap = super.keymap.
    // TODO: disable other commands (like save-buffer) or factor them out of editing mode? or
    // maybe everything just fails because we reject minibuffer use while in the minibuffer... it
    // looks like that might be what emacs does
    bind("abort", "C-g");

  @Fn("""Aborts the current minibuffer action.""")
  def abort () :Unit = {
    window.emitStatus("Quit")
    promise.fail(Errors.feedback("Aborted"))
  }

  /** Sets contents of the minibuffer to `text`. Positions the point at the end of the text. */
  protected def setContents (text :String) :Unit = setContents(Line.fromText(text))

  /** Sets contents of the minibuffer to `lines`. Positions the point at the end of the buffer. */
  protected def setContents (lines :Seq[LineV]) :Unit = {
    view.buffer.replace(view.buffer.start, view.buffer.end, lines)
    if (!lines.isEmpty) view.width() = math.max(view.width(), lines.map(_.length).max+1)
    view.point() = view.buffer.end
  }

  protected def contents :String = Line.toText(view.buffer.lines)
}
