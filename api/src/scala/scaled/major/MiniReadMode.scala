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

  private def current = Line.toText(view.buffer.region(view.buffer.start, view.buffer.end))
  private var _comp = Completion.empty[T]("")
  setCompletion(completer(current))

  // machinery for handling coalesced search string refreshing
  private var _refreshConn = Closeable.Noop
  private def queueRefresh () {
    _refreshConn.close()
    _refreshConn = window.exec.ui.schedule(75, () => {
      val glob = current ; val oglob = _comp.glob
      if (glob != oglob) setCompletion(completer.refine(_comp, glob))
      _refreshConn = Closeable.Noop
    })
  }
  buffer.edited onEmit queueRefresh

  private var nonHistoryText = initText
  private var historyAge = -1
  // reset to historyAge -1 whenever the buffer is modified
  buffer.edited.onEmit { historyAge = -1 }

  override def keymap = super.keymap.
    bind("previous-history-entry", "UP").
    bind("next-history-entry",     "DOWN").
    bind("extend-completion",      "TAB").
    bind("commit-read",            "ENTER");

  @Fn("Extends the current completion to the longest shared prefix of the displayed completions.")
  def extendCompletion () {
    // if we have at least one completion which prefix-matches our current text...
    val preMatchedComps = _comp.comps.filter(Completer.startsWithI(current))
    if (!preMatchedComps.isEmpty) {
      // ...set the contents to the longest shared prefix of the applicable completions
      setContents(Completer.longestPrefix(preMatchedComps))
    }
  }

  @Fn("Commits the current minibuffer read with its current contents.")
  def commitRead () {
    completer.commit(_comp, current).foreach { result =>
      // only add contents to history if it's non-empty
      val lns = buffer.region(buffer.start, buffer.end)
      if (lns.size > 1 || lns.head.length > 0) history.filterAdd(lns)
      promise.succeed(result)
    }
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

  private def showHistory (age :Int) {
    if (age == -1) setContents(nonHistoryText)
    else {
      // if we were showing non-history text, save it before overwriting it with history
      if (historyAge == -1) nonHistoryText = buffer.region(buffer.start, buffer.end)
      setContents(history.entry(age).get)
    }
    // update historyAge after setting contents because setting contents wipes historyAge
    historyAge = age
  }

  private def setCompletion (compF :Future[Completion[T]]) = compF.onSuccess(comp => {
    _comp = comp
    // don't display completions if we're not completing
    // (TODO: have this be a member on Completer; comparing against a global is pretty hacky)
    if (completer != Completer.none) {
      setContents(comp.glob)
      val comps = comp.comps
      if (comps.isEmpty) miniui.showCompletions(Seq("No match."))
      else {
        // if we have a path separator, strip off the path prefix shared by the current completion;
        // this results in substantially smaller and more readable completions when we're
        // completing things like long file system paths
        val pre = comp.glob
        val preLen = completer.pathSeparator map(sep => pre.lastIndexOf(sep)+1) getOrElse 0
        val stripped = if (preLen > 0) comps.map(_.substring(preLen)) else comps
        miniui.showCompletions(stripped)
      }
    }
  })
}
