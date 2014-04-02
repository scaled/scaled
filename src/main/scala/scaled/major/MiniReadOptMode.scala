//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.Promise

import scaled._

@Major(name="mini-readopt", tags=Array("mini"), desc="""
  A minibuffer mode that queries the user for a single key press from a large(ish) selection. In
  addition to the supplied options, C-h will be bound to a fn that displays the help text in the
  minibuffer completion area.
""")
class MiniReadOptMode (
  editor    :Editor,
  config    :Config,
  view      :RBufferView,
  disp      :Dispatcher,
  miniui    :MiniUI,
  promise   :Promise[String],
  prompt    :String,
  /** A map from key trigger (e.g. `y`, `C-r`, `!`, etc.) to a help string for the option. */
  opts      :Seq[(String,String)]
) extends MinibufferMode(editor, config, view, disp, promise) {

  val optMap = opts.toMap
  def optprompt = prompt + opts.map(_._1).mkString(" (", ", ", ", C-h)")
  miniui.setPrompt(optprompt)

  override def keymap = Seq(
    "C-g" -> "abort",
    "C-h" -> "show-help"
  )

  // disable our default fn; route everything to unknownCommand (our missedFn)
  override def defaultFn = None

  override def unknownCommand (trigger :String) = {
    if (optMap.contains(trigger)) promise.succeed(trigger)
    else editor.emitStatus("Type C-h for help.")
  }

  @Fn("Displays the option descriptions in the minibuffer completion area.")
  def showHelp () {
    val maxWidth = opts.map(_._1).map(_.length).max
    miniui.showCompletions(opts map { case (k, v) =>
      String.format(s"%-${maxWidth}s - %s", k, v)
    })
  }
}
