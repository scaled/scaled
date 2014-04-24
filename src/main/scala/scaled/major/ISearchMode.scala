//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import reactual.{Value, Promise}
import scaled._

/** Configuration for [[ISearchMode]]. */
object ISearchConfig extends Config.Defs {

  @Var("The number of entries retained by the recent searches ring.")
  val isearchRingSize = key(40)
  /** The ring in which recent searches are stored. */
  val isearchRing = fnKey(cfg => new Ring(cfg(isearchRingSize)))

  /** The CSS style applied to isearch matches. */
  val isearchMatchStyle = "isearchMatchFace"

  /** The CSS style applied to isearch matches. */
  val isearchActiveMatchStyle = "isearchActiveMatchFace"
}

@Major(name="mini-isearch", tags=Array("mini"), desc="""
  A minibuffer mode that handles interactive searching, forward and back.
""")
class ISearchMode (
  env       :Env,
  miniui    :MiniUI,
  promise   :Promise[Unit],
  mainView  :RBufferView,
  mainDisp  :Dispatcher,
  direction :String
) extends MinibufferMode(env, promise) {
  import ISearchConfig._

  @inline protected final def mainBuffer = mainView.buffer

  // we track the state of our isearch as a stack of states; every change pushes a new state on the
  // stack and DEL pops back to previous states
  case class State (sought :Seq[LineV], matches :Seq[Loc], start :Loc, end :Loc,
                    fwd :Boolean, fail :Boolean, wrap :Boolean) extends Region {

    /** The location to place the point when this state is active. */
    def point :Loc = if (fwd) end else start

    /** Returns the prompt string that should be used for this state. */
    def prompt :String = {
      val buf = new StringBuilder
      if (fail) buf.append("failing ")
      if (wrap) buf.append("wrapped ")
      buf.append("I-search")
      if (!fwd) buf.append(" backward")
      matches.size match {
        case 0 => // nada
        case 1 => buf.append(" (1 match)")
        case n => buf.append(" (").append(n).append(" matches)")
      }
      buf.append(":")
      buf.setCharAt(0, Character.toUpperCase(buf.charAt(0)))
      buf.toString
    }

    /** Returns true if we should highlight non-active matches. We avoid highlighting single char
      * searches because it's not particularly useful, and it needlessly slows down the initial
      * search process. */
    def shouldShowMatches :Boolean = sought.size > 1 || sought.head.length > 1

    /** Applies this state, highlighting matches and the active match (if any), moving the point
      * appropriately and configuring the contents of the minibuffer.
      * @param prev the previously active state, which will be unapplied (efficiently such that if
      * this state and the previous have the same configuration, no change is made).
      */
    def apply (prev :State) {
      // we use `ne` here for efficiency because the only time things are meaningfully equal is
      // when we've inherited the exact sought text and matches from a previous state
      if ((prev.sought ne sought) || (prev.matches ne matches)) {
        prev.clearMatches()
        if (shouldShowMatches) matches foreach {
          l => mainBuffer.addStyle(isearchMatchStyle, l, l + sought) }
        setContents(sought)
      }
      if (prev.start != start || prev.end != end) {
        mainBuffer.removeStyle(isearchActiveMatchStyle, prev)
        mainBuffer.addStyle(isearchActiveMatchStyle, this)
      }
      mainView.point() = point
      miniui.setPrompt(prompt)
    }

    /** Clears the highlights applied by this state. */
    def clear () {
      clearMatches()
      mainBuffer.removeStyle(isearchActiveMatchStyle, this)
    }

    def extend (esought :Seq[LineV]) = {
      val ematches = mainBuffer.search(esought, mainBuffer.start, mainBuffer.end)
      find(esought, ematches, if (fwd) start else end, fwd, wrap)
    }

    def next () = {
      val newwrap = fwd && fail
      val from = if (newwrap) mainBuffer.start else point
      find(sought, matches, from, true, wrap || newwrap)
    }

    def prev () = {
      val newwrap = !fwd && fail
      val from = if (newwrap) mainBuffer.end else point
      find(sought, matches, from, false, wrap || newwrap)
    }

    protected def clearMatches () :Unit = if (shouldShowMatches) matches foreach { l =>
      mainBuffer.removeStyle(isearchMatchStyle, l, l + sought) }

    protected def find (sought :Seq[LineV], matches :Seq[Loc], from :Loc,
                        fwd :Boolean, wrap :Boolean) = {
      val found = if (fwd) matches.dropWhile(_ < from).headOption
                  else matches.takeWhile(_ < from).lastOption
      found match {
        case Some(s) => State(sought, matches, s, s+sought, fwd, false, wrap)
        case None    => State(sought, matches, start, end,  fwd, true,  wrap)
      }
    }
  }

  private val initState = {
    val p = mainView.point()
    new State(Line.fromText(""), Seq(), p, p, direction == "forward", false, false)
  }
  miniui.setPrompt(initState.prompt)
  private var _states = List(initState)
  private def curstate = _states.head

  private def pushState (state :State) {
    state.apply(curstate)
    _states = state :: _states
  }
  private def popState () :Unit = _states match {
    case cur :: prev :: t => prev.apply(cur) ; _states = prev :: t
    case _ => editor.popStatus("Can't pop empty isearch state stack!")
  }

  // machinery for handling coalesced search term refreshing
  private var _refreshPending = false
  private def queueRefresh () {
    if (!_refreshPending) {
      _refreshPending = true
      editor.defer {
        _refreshPending = false
        val sought = buffer.region(buffer.start, buffer.end)
        if (sought != curstate.sought) pushState(curstate.extend(sought))
      }
    }
  }
  buffer.edited onEmit queueRefresh

  override def configDefs = ISearchConfig :: super.configDefs
  override def keymap = Seq(
    // we don't inherit normal editing commands, so we repeat the few essentials here and then
    // route all non-matching commands back to the main buffer
    "C-g"   -> "abort",
    "C-y"   -> "yank",
    "M-y"   -> "yank-pop",

    "C-s"   -> "next-match",
    "C-r"   -> "prev-match",

    "BS"    -> "prev-search",
    "DEL"   -> "prev-search",
    "ENTER" -> "end-search"
    // Type C-j to match end of line.
    // Type C-w to yank next word or character in buffer onto the end of the search string.
    // Type M-s C-e to yank rest of line onto end of search string and search for it.
    // Type C-q to quote control character to search for it.
  )

  override def dispose () {
    super.dispose()
    curstate.clear() // clear any highlights
  }

  // when a non-isearch key binding is pressed...
  override def unknownCommand (trigger :String) {
    endSearch()             // terminate the search normally...
    mainDisp.press(trigger) // and "execute" the pressed key in the main buffer
  }

  override def abort () {
    // if we're currently failing, peel back to the last successful state
    if (curstate.fail) {
      val oldcur = curstate
      while (curstate.fail) _states = _states.tail
      curstate.apply(oldcur)
    }
    // otherwise revert the main buffer point to its initial value and abort the search
    else {
      mainView.point() = initState.point
      super.abort()
    }
  }

  @Fn("Ends this search, leaving the point at the location found.")
  def endSearch () :Unit = {
    if (curstate ne initState) config(isearchRing).add(curstate.sought)
    promise.succeed(())
  }

  @Fn("Cancels the last change to the isearch, reverting to the previous search.")
  def prevSearch () {
    if (_states.tail != Nil) popState()
    else editor.popStatus("No previous search.")
  }

  @Fn("""Moves forward to the next occurrance of the current matched text, if any.
         The point will be placed immediately after the match.""")
  def nextMatch () {
    // if we're not at the default state, or we're changing direction, next() the current state
    if (curstate != initState || !curstate.fwd) pushState(curstate.next())
    // otherwise populate the search with the first entry from the search history ring
    else setFromHistory(0)
  }

  @Fn("""Moves backward to the previous occurrance of the current matched text, if any.
         The point will be placed at the start of the match.""")
  def prevMatch () {
    // if we're not at the default state, or we're changing direction, prev() the current state
    if (curstate != initState || curstate.fwd) pushState(curstate.prev())
    // otherwise populate the search with the first entry from the search history ring
    else setFromHistory(0)
  }

  protected def setFromHistory (idx :Int) :Unit = config(isearchRing).entry(0) match {
    case Some(sought) => setContents(sought)
    case None => editor.popStatus("No previous search string.")
  }
}
