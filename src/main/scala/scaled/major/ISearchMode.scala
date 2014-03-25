//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.tailrec

import reactual.Promise
import scaled._

/** Configuration for [[ISearchMode]]. */
object ISearchConfig extends ConfigDefs {

  /** The CSS style applied to isearch matches. */
  val isearchMatchStyle = "isearchMatchFace"

  /** The CSS style applied to isearch matches. */
  val isearchActiveMatchStyle = "isearchActiveMatchFace"
}

/** A minibuffer mode that handle interactive searching, both forward and back.
  */
class ISearchMode (
  editor    :Editor,
  config    :Config,
  miniView  :RBufferView,
  disp      :Dispatcher,
  miniui    :MiniUI,
  promise   :Promise[Unit],
  mainView  :RBufferView,
  direction :String
) extends MinibufferMode(editor, config, miniView, disp, promise) {
  import ISearchConfig._

  private var _fwd = direction == "forward"
  private def dirsuff = if (_fwd) "" else " backward"
  private def reprompt (pref :String) = miniui.setPrompt(s"${pref}I-search${dirsuff}:")
  reprompt("")

  // make a note of the initial point, we go back there if aborted
  private val initPoint = mainView.point()

  // machinery for manipulating the highlighted matches
  private var _sought = Seq[LineV]()
  private var _matches = Seq[Loc]()
  private def updateMatches (sought :Seq[LineV], matches :Seq[Loc]) {
    // remove the matched face from the previous matches
    if (!_matches.isEmpty) _matches foreach { l =>
      mainView.buffer.removeStyle(isearchMatchStyle, l, l + _sought)
    }
    _sought = sought
    _matches = matches
    // add the match style to the new matches
    if (!matches.isEmpty) matches foreach { l =>
      mainView.buffer.addStyle(isearchMatchStyle, l, l + sought)
    }
  }

  private var _activeMatch :Option[(Loc,Loc)] = None
  private def updateActiveMatch (loc :Option[Loc]) {
    _activeMatch foreach { case (s, e) =>
      mainView.buffer.removeStyle(isearchActiveMatchStyle, s, e) }
    _activeMatch = loc map(l => (l, l + _sought))
    _activeMatch foreach { case (s, e) =>
      mainView.buffer.addStyle(isearchActiveMatchStyle, s, e)
      mainView.point() = e
    }
  }

  // machinery for handling coalesced search term refreshing
  private var _refreshPending = false
  private def queueRefresh () {
    if (!_refreshPending) {
      _refreshPending = true
      editor.defer {
        _refreshPending = false
        refresh()
      }
    }
  }
  miniView.buffer.edited onEmit queueRefresh
  miniView.buffer.lineEdited onEmit queueRefresh

  private def preserveActiveMatch = _activeMatch match {
    case None => false
    case Some((s, e)) => _matches.contains(s)
  }
  private def extendActiveMatch () = updateActiveMatch(Some(_activeMatch.get._1))

  private def refresh () {
    val term = miniView.buffer.region(miniView.buffer.start, miniView.buffer.end)
    updateMatches(term, mainView.buffer.search(term, mainView.buffer.start, mainView.buffer.end))
    if (preserveActiveMatch) extendActiveMatch()
    else if (_fwd) nextMatch() else prevMatch()
  }

  override def nameSuffix = "isearch"
  override def configDefs = ISearchConfig :: super.configDefs
  override def keymap = Seq(
    // we don't inherit normal editing commands, so we repeat the few essentials here and then
    // route all non-matching commands back to the main buffer
    "C-g"   -> "abort",
    "BS"    -> "delete-backward-char", // TODO: do we need to do anything special?
    "C-y"   -> "yank",
    "M-y"   -> "yank-pop",

    "C-s"   -> "next-match",
    "C-r"   -> "prev-match",

    "ENTER" -> "end-search"
    // Type DEL to cancel last input item from end of search string.
    // Type RET to exit, leaving point at location found.
    // Type C-j to match end of line.
    // Type C-s to search again forward, C-r to search again backward.
    // Type C-w to yank next word or character in buffer onto the end of the search string.
    // Type M-s C-e to yank rest of line onto end of search string and search for it.
    // Type C-q to quote control character to search for it.
  )

  override def dispose () {
    super.dispose()
    updateActiveMatch(None)
    updateMatches(Seq(), Seq())
  }

  override def abort () {
    // TODO: if we have an unmatched search, peel back to the last match (which may be nothing)

    // otherwise revert the main buffer point to its initial value and abort the search
    mainView.point() = initPoint
    super.abort()
  }

  def nextMatch (from :Loc) :Boolean = {
    _fwd = true
    _matches.dropWhile(_ < from).headOption match {
      case Some(loc) => updateActiveMatch(Some(loc)) ; true
      case None => updateActiveMatch(None) ; false
    }
  }

  @Fn("Ends this search, leaving the point at the location found.")
  def endSearch () :Unit = promise.succeed(())

  @Fn("""Moves forward to the next occurrance of the current matched text, if any.
         The point will be placed immediately after the match.""")
  def nextMatch () {
    if (!nextMatch(mainView.point()))
      reprompt("Failing ") // TODO: wrap, "Failing wrapped " on wrapped fail
  }

  @Fn("""Moves backward to the previous occurrance of the current matched text, if any.
         The point will be placed at the start of the match.""")
  def prevMatch () {
    _fwd = false
  }
}
