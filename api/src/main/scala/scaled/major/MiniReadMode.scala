//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

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
  history   :Ring,
  completer :Completer[T]
) extends MinibufferMode(env, promise) {

  miniui.setPrompt(prompt)
  setContents(initText)

  override def keymap = super.keymap.
    bind("UP",    "previous-history-entry").
    bind("DOWN",  "next-history-entry").
    bind("TAB",   "complete").
    bind("S-TAB", "complete").
    // TODO: special C-d that updates completions if at eol (but does not complete max prefix)
    bind("ENTER", "commit-read");

  def current = Line.toText(view.buffer.region(view.buffer.start, view.buffer.end))

  @Fn("Commits the current minibuffer read with its current contents.")
  def commitRead () {
    val curpre = current
    // if they commit and we have no current completion...
    if (!curcomp.isDefined) {
      // generate a completion and then attempt to commit with that; if they provided a valid
      // completion (or more likely it was provided as a default) then it will commit directly
      curcomp = Some(completer.complete(curpre))
      // don't allow the default completion to be used because the user hasn't seen it yet
      completer.commit(curcomp, curpre, false) match {
        // if no match, display the completion to let them know we need more info
        case None    => display(curpre, curcomp.get)
        case Some(r) => succeed(r)
      }
    }
    // otherwise we commit with the current completion
    else completer.commit(curcomp, curpre, true) match {
      case None    => complete()
      case Some(r) => succeed(r)
    }
  }

  // saves the current contents to our history and sends back a resultult
  private def succeed (result :T) {
    // only add contents to history if it's non-empty
    val lns = buffer.region(buffer.start, buffer.end)
    if (lns.size > 1 || lns.head.length > 0) history.filterAdd(lns)
    promise.succeed(result)
  }

  private var curcomp :Option[Completion[T]] =  None
  private var currentText = initText
  private var historyAge = -1
  // reset to historyAge -1 whenever the buffer is modified
  buffer.edited.onEmit { historyAge = -1 }

  private def showHistory (age :Int) {
    if (age == -1) setContents(currentText)
    else {
      // if we were showing currentText, save it before overwriting it with history
      if (historyAge == -1) currentText = buffer.region(buffer.start, buffer.end)
      setContents(history.entry(age).get)
    }
    // update historyAge after setting contents because setting contents wipes historyAge
    historyAge = age
  }

  @Fn("Puts the previous history entry into the minibuffer.")
  def previousHistoryEntry () {
    val prevAge = historyAge+1
    if (prevAge >= history.entries) window.popStatus("Beginning of history; no preceding item")
    else showHistory(prevAge)
  }

  @Fn("Puts the next history entry into the minibuffer.")
  def nextHistoryEntry () {
    if (historyAge == -1) window.popStatus("End of history")
    else showHistory(historyAge-1)
  }

  @Fn("Completes the minibuffer contents as much as possible.")
  def complete () {
    val curpre = current
    val comp = completer.complete(curpre)
    curcomp = Some(comp)
    display(curpre, comp)
  }

  private def display (curpre :String, comp :Completion[_]) {
    if (comp.comps.isEmpty) miniui.showCompletions(Seq("No match."))
    else if (comp.comps.size == 1) {
      miniui.showCompletions(Seq())
      setContents(comp.comps.head)
    }
    else {
      // allow the completer to massage the currently displayed prefix (usually this means
      // replacing it with the longest shared prefix of the completions)
      val pre = comp.massageCurrent(curpre)
      if (pre != curpre) setContents(pre)
      // if we have a path separator, strip off the path prefix shared by the current completion;
      // this results in substantially smaller and more readable completions when we're completing
      // things like long file system paths
      val preLen = completer.pathSeparator map(sep => pre.lastIndexOf(sep)+1) getOrElse 0
      val stripped = if (preLen > 0) comp.comps.map(_.substring(preLen)) else comp.comps
      miniui.showCompletions(stripped)
    }
  }

  private def copyContents = buffer.region(buffer.start, buffer.end)
}
