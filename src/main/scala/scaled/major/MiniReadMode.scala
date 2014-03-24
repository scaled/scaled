//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec

import reactual.Promise
import scaled._

/** A minibuffer mode that queries the user for a string, using a supplied completion function to
  * allow the user to tab-complete their way to satisfaction.
  */
class MiniReadMode (
  editor    :Editor,
  config    :Config,
  view      :RBufferView,
  disp      :Dispatcher,
  prompt    :MiniPrompt,
  promise   :Promise[String],
  defPrompt :String,
  defText   :Seq[LineV],
  completer :String => Set[String]
) extends MinibufferMode(editor, config, view, disp, promise) {

  prompt.set(defPrompt)
  setContents(defText)

  override def nameSuffix = "read"

  override def keymap = super.keymap ++ Seq(
    "ENTER" -> "commit-read",
    "TAB"   -> "complete",
    "S-TAB" -> "complete"
    // TODO: history commands
  )

  @Fn("Commits the current minibuffer read with its current contents.")
  def commitRead () {
    promise.succeed(contents)
  }

  @Fn("Completes the minibuffer contents as much as possible.")
  def complete () {
    val current = mkString(view.buffer.region(view.buffer.start, view.buffer.end))
    val comps = completer(current)
    if (comps.isEmpty) {
      view.popup() = Popup(Seq("No match."), Popup.DnRight(0, 0), true)
    }
    else if (comps.size == 1) {
      view.popup.clear()
      setContents(comps.head)
    }
    else {
      view.popup() = Popup(comps.toSeq.sorted, Popup.DnRight(0, 0), false)
      val pre = longestPrefix(comps)
      if (pre != current) setContents(pre)
    }
  }

  private def sharedPrefix (a :String, b :String) = if (b startsWith a) a else {
    val buf = new StringBuilder
    @tailrec def loop (ii :Int) {
      if (ii < a.length && ii < b.length && a.charAt(ii) == b.charAt(ii)) {
        buf.append(a.charAt(ii))
        loop(ii+1)
      }
    }
    loop(0)
    buf.toString
  }
  private def longestPrefix (comps :Set[String]) = comps reduce sharedPrefix
}
