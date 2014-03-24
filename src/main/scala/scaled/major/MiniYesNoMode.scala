//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.Promise

import scaled._

/** A minibuffer mode that queries the user for a string, using a supplied completion function to
  * allow the user to tab-complete their way to satisfaction.
  */
class MiniYesNoMode (
  editor    :Editor,
  config    :Config,
  view      :RBufferView,
  disp      :Dispatcher,
  prompt    :MiniPrompt,
  promise   :Promise[Boolean],
  defPrompt :String
) extends MinibufferMode(editor, config, view, disp, promise) {

  prompt.set(defPrompt)

  override def nameSuffix = "yesno"

  override def selfInsertCommand (typed :String) = typed match {
    case "y" => promise.succeed(true)
    case "n" => promise.succeed(false)
    case _   => prompt.set(s"Please answer y or n. $defPrompt")
  }
}
