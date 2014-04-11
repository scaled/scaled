//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

import reactual.Promise
import scaled._

@Major(name="mini-read", tags=Array("mini"), desc="""
  A minibuffer mode that queries the user for a string, using a supplied completion function to
  allow the user to tab-complete their way to satisfaction.
""")
class MiniReadMode[T] (
  env       :Env,
  miniui    :MiniUI,
  promise   :Promise[T],
  prompt    :String,
  initText  :Seq[LineV],
  completer :Completer[T]
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
    completer.commit(current) match {
      case Some(result) => promise.succeed(result)
      case None         => complete()
    }
  }

  @Fn("Completes the minibuffer contents as much as possible.")
  def complete () {
    val cur = current
    val comps = completer(cur).keySet
    if (comps.isEmpty) miniui.showCompletions(Seq("No match."))
    else if (comps.size == 1) {
      miniui.showCompletions(Seq())
      setContents(comps.head)
    }
    else {
      // replace the current contents with the longest shared prefix of the completions
      val pre = longestPrefix(comps)
      if (pre != cur) setContents(pre)
      // if we have a path separator, strip off the path prefix shared by the current completion;
      // this results in substantially smaller and more readable completions when we're completing
      // things like long file system paths
      val preLen = completer.pathSeparator map(sep => pre.lastIndexOf(sep)+1) getOrElse 0
      miniui.showCompletions(comps.map(_.substring(preLen)).toSeq)
    }
  }

  private def sharedPrefix (a :String, b :String) = if (b startsWith a) a else {
    val buf = new StringBuilder
    @inline @tailrec def loop (ii :Int) {
      if (ii < a.length && ii < b.length) {
        val ra = a.charAt(ii) ; val la = Character.toLowerCase(ra)
        val rb = b.charAt(ii) ; val lb = Character.toLowerCase(rb)
        if (la == lb) {
          // if everyone uses uppercase here, keep the prefix uppercase, otherwise lower
          val c = if (Character.isUpperCase(ra) && Character.isUpperCase(rb)) ra else la
          buf.append(c)
          loop(ii+1)
        }
      }
    }
    loop(0)
    buf.toString
  }
  private def longestPrefix (comps :Iterable[String]) = comps reduce sharedPrefix
}
