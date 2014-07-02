//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import reactual.{Value, Future, Promise}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
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

  case class State (sought :Seq[LineV], matchesF :Future[Seq[Loc]], start :Loc, end :Loc,
                    fwd :Boolean, fail :Boolean, wrap :Boolean) extends Region {

    private var matches :Option[Seq[Loc]] = None
    private def setMatches (matches :Seq[Loc]) {
      this.matches = Some(matches)
      if (curstate eq this) {
        miniui.setPrompt(prompt)
        showMatches()
      }
    }
    matchesF.onSuccess(setMatches)

    /** The location to place the point when this state is active. */
    def point :Loc = if (fwd) end else start

    /** Returns the prompt string that should be used for this state. */
    def prompt :String = {
      val buf = new StringBuilder
      if (fail) buf.append("failing ")
      if (wrap) buf.append("wrapped ")
      buf.append("I-search")
      if (!fwd) buf.append(" backward")
      matches.foreach { _.size match {
        case 0 => // nada
        case 1 => buf.append(" (1 match)")
        case n => buf.append(" (").append(n).append(" matches)")
      }}
      buf.append(":")
      buf.setCharAt(0, Character.toUpperCase(buf.charAt(0)))
      buf.toString
    }

    /** Applies this state, highlighting matches and the active match (if any), moving the point
      * appropriately and configuring the contents of the minibuffer.
      * @param prev the previously active state, which will be unapplied (efficiently such that if
      * this state and the previous have the same configuration, no change is made).
      */
    def apply (prev :State) {
      if (prev.sought ne sought) {
        prev.clearMatches()
        showMatches()
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
      val search = mkSearch(esought)
      val allMatches = env.exec.runAsync(search.findAll())
      (if (fwd) search.findForward(start) else search.findBackward(end)) match {
        case Loc.None => State(esought, allMatches, start, end,  fwd, true,  wrap)
        case s        => State(esought, allMatches, s, s+esought, fwd, false, wrap)
      }
    }

    def next () = {
      val newwrap = fwd && fail
      val from = if (newwrap) mainBuffer.start else point
      // if our matches are not yet available, we need to search the buffer
      val next = if (!matches.isDefined) mkSearch(sought).findForward(from)
                 else matches.get.find(_ >= from) getOrElse Loc.None
      advance(next, true, wrap || newwrap)
    }

    def prev () = {
      val newwrap = !fwd && fail
      val from = if (newwrap) mainBuffer.end else point
      // if our matches are not yet available, we need to search the buffer
      val next = if (!matches.isDefined) mkSearch(sought).findBackward(from)
                 else matches.get.takeWhile(_ < from).lastOption getOrElse Loc.None
      advance(next, false, wrap || newwrap)
    }

    override def toString = {
      val ms = matches.map(_.size).getOrElse(-1) ; val d = if (fwd) "fwd" else "rev"
      val f = if (fail) "fail" else "succ" ; val w = if (wrap) "wrap" else "nowrap"
      s"State($sought, $ms, [$start, $end), $d, $f, $w)"
    }

    protected def mkSearch (sought :Seq[LineV]) = Search(
      mainBuffer, mainBuffer.start, mainBuffer.end, sought)

    // TODO: defer showMatches for 250-500ms?
    protected def showMatches () :Unit = if (matches.isDefined) matches.get foreach { l =>
      mainBuffer.addStyle(isearchMatchStyle, l, l + sought) }
    protected def clearMatches () :Unit = if (matches.isDefined) matches.get foreach { l =>
      mainBuffer.removeStyle(isearchMatchStyle, l, l + sought) }

    protected def advance (next :Loc, fwd :Boolean, wrap :Boolean) =
      if (next == Loc.None) State(sought, matchesF, start, end,        fwd, true,  wrap)
      else                  State(sought, matchesF, next, next+sought, fwd, false, wrap)
  }

  // we track the state of our isearch as a stack of states
  private val initState = {
    val p = mainView.point()
    // we never "complete" the initial state's search future; we can't provide a future that
    // completes immediately because that would cause State to check whether it's the curstate, but
    // we haven't initialized _states yet so curstate can't be called; we could complete the initial
    // state asynchronously but there's no particular benefit to doing so
    State(Seq(Line.Empty), Promise(), p, p, direction == "forward", false, false)
  }
  miniui.setPrompt(initState.prompt)
  private var _states = List(initState)
  private def curstate = _states.head

  // every change pushes a new state on the stack
  private def pushState (state :State) {
    state.apply(curstate)
    _states = state :: _states
  }
  // DEL pops back to previous states
  private def popState () :Unit = _states match {
    case cur :: prev :: t => prev.apply(cur) ; _states = prev :: t
    case _ => editor.popStatus("Can't pop empty isearch state stack!")
  }

  // machinery for handling coalesced search term refreshing
  private var _refreshPending = false
  private def queueRefresh () {
    if (!_refreshPending) {
      _refreshPending = true
      env.exec.runOnUI {
        val sought = buffer.region(buffer.start, buffer.end)
        if (sought != curstate.sought) pushState(curstate.extend(sought))
        _refreshPending = false
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
