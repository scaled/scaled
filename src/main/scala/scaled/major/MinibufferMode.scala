//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.Promise

import scaled._

/** Enable the reading and setting of the minibuffer prompt string. This is a special dependency
  * that can be injected by [[MinibufferMode]]s. */
abstract class MiniPrompt {

  /** Sets the minibuffer prompt to `prompt`. */
  def set (prompt :String) :Unit

  /** Returns the current minibuffer prompt. */
  def get :String
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
  */
abstract class MinibufferMode (
  editor      :Editor,
  config      :Config,
  view        :RBufferView,
  disp        :Dispatcher,
  promise     :Promise[_]
) extends EditingMode(editor, config, view, disp) {

  /** All minibuffer modes are named `mini-something`. This method defines `something`. */
  def nameSuffix :String

  final override def name = s"mini-$nameSuffix"

  override def keymap = super.keymap ++ Seq(
    "C-g" -> "abort"
    // TODO: disable other commands (like save-buffer) or factor them out of editing mode? or
    // maybe everythign just fails because we reject minibuffer use while in the minibuffer... it
    // looks like that might be what emacs does
  )

  override def dispose () {} // nada

  @Fn("""Aborts the current minibuffer action.""")
  def abort () {
    editor.emitStatus("Quit")
    promise.fail(new Exception("Aborted"))
  }

  /** Sets contents of the minibuffer to `text`. Positions the point at the end of the text. */
  protected def setContents (text :String) :Unit = setContents(Line.fromText(text))

  /** Sets contents of the minibuffer to `lines`. Positions the point at the end of the buffer. */
  protected def setContents (lines :Seq[LineV]) {
    view.buffer.replace(view.buffer.start, view.buffer.end, lines)
    view.point = view.buffer.end
  }

  protected def contents :String = mkString(view.buffer.region(view.buffer.start, view.buffer.end))

  /** Converts a sequence of lines to a string. TODO: what about line endings? */
  protected def mkString (lines :Seq[Line]) = lines.map(_.asString).mkString("\n")
}
