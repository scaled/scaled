//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.Promise

import scaled._

@Mode(name="mini-yesno",
      desc="A minibuffer mode that asks the user to respond 'y' or 'n' to a question.")
class MiniYesNoMode (
  editor    :Editor,
  config    :Config,
  view      :RBufferView,
  disp      :Dispatcher,
  miniui    :MiniUI,
  promise   :Promise[Boolean],
  prompt    :String
) extends MinibufferMode(editor, config, view, disp, promise) {

  def ynprompt = s"$prompt (y or n)"
  miniui.setPrompt(ynprompt)

  override def selfInsertCommand (typed :String) = typed match {
    case "y" => promise.succeed(true)
    case "n" => promise.succeed(false)
    case _   => miniui.setPrompt(s"Please answer y or n. $ynprompt")
  }
}
