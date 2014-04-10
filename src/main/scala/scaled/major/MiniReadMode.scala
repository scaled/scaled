//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec

import reactual.Promise
import scaled._

@Major(name="mini-read", tags=Array("mini"), desc="""
  A minibuffer mode that queries the user for a string, using a supplied completion function to
  allow the user to tab-complete their way to satisfaction.
""")
class MiniReadMode (
  env       :Env,
  miniui    :MiniUI,
  promise   :Promise[String],
  prompt    :String,
  initText  :Seq[LineV],
  completer :Completer
) extends MinibufferMode(env, promise) {

  miniui.setPrompt(prompt)
  setContents(initText)

  override def keymap = super.keymap ++ Seq(
    "TAB"   -> "complete",
    "S-TAB" -> "complete",
    // TODO: special C-d that updates completions if at eol (but does not complete max prefix)
    // TODO: history commands
    "ENTER" -> "commit-read"
  )

  def current = mkString(view.buffer.region(view.buffer.start, view.buffer.end))

  @Fn("Commits the current minibuffer read with its current contents.")
  def commitRead () {
    val cur = current
    if (completer.requireCompletion && !completer(cur)(cur)) complete()
    else promise.succeed(contents)
  }

  @Fn("Completes the minibuffer contents as much as possible.")
  def complete () {
    val cur = current
    val comps = completer(cur)
    if (comps.isEmpty) miniui.showCompletions(Seq("No match."))
    else if (comps.size == 1) {
      miniui.showCompletions(Seq())
      setContents(comps.head)
    }
    else {
      // if we have a path separator, strip off the path prefix shared by the current completion;
      // this results in substantially smaller and more readable completions when we're completing
      // things like long file system paths
      val preLen = completer.pathSeparator map(sep => cur.lastIndexOf(sep)+1) getOrElse 0
      miniui.showCompletions(comps.toSeq.map(_.substring(preLen)).sorted)
      val pre = longestPrefix(comps)
      if (pre != cur) setContents(pre)
    }
  }

  private def sharedPrefix (a :String, b :String) = if (b startsWith a) a else {
    val buf = new StringBuilder
    @inline @tailrec def loop (ii :Int) {
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
