//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.util.regex.{MatchResult, Pattern}
import scala.annotation.tailrec
import scaled.util.Chars

/** Handles searching and matching in [[Line]]s. */
abstract class Matcher {

  /** Searches forward for a match in `[begin,end)` of `haystack` starting at `begin`.
    * @return the index at which a match occurred or -1. */
  def search (haystack :Array[Char], begin :Int, end :Int) :Int

  /** Searches backward for a match in `[begin,end)` of `haystack` starting at `from`.
    * @return the index at which a match occurred or -1. */
  def searchBackward (haystack :Array[Char], begin :Int, end :Int, from :Int) :Int

  /** Returns true if this matcher matches a prefix of `haystack` starting at `begin` without
    * extending to or beyond `end`. */
  def matches (haystack :Array[Char], begin :Int, end :Int) :Boolean

  /** Returns the length of the most recent match. This method must be preceeded by a call to
    * [[search]], [[searchBackward]] or [[matches]] which returned a successful match, otherwise
    * its return value is undefined. */
  def matchLength :Int

  /** Replaces a match from this matcher in `buffer` at `at` with `lines`.
    * @return the location immediately following the replaced text. */
  def replace (buffer :Buffer, at :Loc, lines :Seq[LineV]) :Loc
}

/** A specialized matcher that searches and matches regular expressions. */
class RegexpMatcher (pattern :String) extends Matcher {

  private val p = Pattern.compile(pattern)
  private val m = p.matcher("")

  // contains the last successful match result; in searchBackward we have to repeatedly apply our
  // matcher looking for the last match in the region, so we store the last successful match here
  // to avoid having to recompute it after we learn that it was the last match by failing a
  // subsequent match
  private var result :MatchResult = _

  // java's Matcher wants to operate on CharSequence, but we have an array, so we create use this
  // reusable facade to avoid creating garbage on every match (assuming java's Matcher doesn't
  // create garbage itself; probably wishful thinking...)
  private var current :Array[Char] = _
  private val curseq = new CharSequence() {
    def length = current.length
    def charAt (ii :Int) = current(ii)
    def subSequence (start :Int, end :Int) = new String(current, start, end-start)
  }

  /** Returns the text matched by the supplied regexp group.
    * This is only valid after a successful match.*/
  def group (number :Int) :String = result.group(number)

  override def search (haystack :Array[Char], begin :Int, end :Int) :Int = {
    prep(haystack, begin, end)
    if (!m.find) -1 else m.start
  }

  def searchBackward (haystack :Array[Char], begin :Int, end :Int, from :Int) :Int = {
    prep(haystack, begin, end)
    var start = begin
    while (start <= from && m.find(start)) {
      result = m.toMatchResult
      start = m.end
    }
    if (start == begin) -1 else result.start
  }

  override def matches (haystack :Array[Char], hfirst :Int, hlast :Int) :Boolean = {
    prep(haystack, hfirst, hlast)
    m.lookingAt
  }

  override def matchLength = result.end - result.start

  override def replace (buffer :Buffer, at :Loc, lines :Seq[LineV]) = {
    val end = buffer.forward(at, matchLength)
    val sb = new StringBuffer()
    m.appendReplacement(sb, Line.toText(lines))
    sb.delete(0, m.start) // sigh...
    val repl = Line.fromText(sb.toString)
    buffer.replace(at, end, repl)
    at + repl
  }

  override def toString = pattern

  private def prep (haystack :Array[Char], start :Int, end :Int) {
    current = haystack
    m.reset(curseq)
    m.region(start, end)
    result = m
  }
}

object Matcher {
  import Chars._

  /** Returns a case-sensitive matcher on `cs`. */
  def exact (cs :CharSequence) :Matcher = new CSMatcher(cs) {
    def replace (buffer :Buffer, at :Loc, lines :Seq[LineV]) = {
      val end = buffer.forward(at, matchLength)
      buffer.replace(at, end, lines)
      at + lines
    }
    protected def eq (have :Char, want :Char) = have == want
    override def toString = super.toString + ":exact"
  }

  /** Returns a case-insensitive matcher on `cs`. NOTE: `cs` must be all lower case. */
  def loose (cs :CharSequence) :Matcher = new CSMatcher(cs) {
    def replace (buffer :Buffer, at :Loc, lines :Seq[LineV]) = {
      val end = buffer.forward(at, matchLength)
      val isCaps = isUpperCase(buffer.charAt(at))
      val isUpper = isCaps && buffer.scanForward(isNotUpperCase, at, end) == end
      buffer.replace(at, end, lines)
      val newend = at + lines
      if (isUpper) buffer.transform(at, newend, Character.toUpperCase)
      else if (isCaps) buffer.transform(at, at.nextC, Character.toUpperCase)
      newend
    }
    protected def eq (have :Char, want :Char) = have == want || have == Character.toLowerCase(want)
    override def toString = super.toString + ":loose"
  }

  /** Returns a regep matcher on `pattern`. */
  def regexp (pattern :String) :RegexpMatcher = new RegexpMatcher(pattern)

  /** Returns matchers for use in searching for `sought`. If `sought` contains all lower-case
    * characters, a case insensitive matchers are returned, otherwise exact case matchers are
    * returned. */
  def on (sought :Seq[LineV]) :Seq[Matcher] =
    sought.map(if (sought.exists(mixedCase(_, 0))) exact else loose)

  /** Returns matchers for use in searching for `sought`. If `sought` contains all lower-case
    * characters, a case insensitive matchers are returned, otherwise exact case matchers are
    * returned. */
  def on (sought :CharSequence) :Matcher =
    if (mixedCase(sought, 0)) exact(sought) else loose(sought)

  private def mixedCase (cs :CharSequence, ii :Int) :Boolean =
    if (ii == cs.length) false
    else if (Character.isUpperCase(cs.charAt(ii))) true
    else mixedCase(cs, ii+1)

  // TODO: if it turns out to be worth it performance-wise, we can create specialized matchers on
  // Line which operate directly on the _chars array; it's probably not worth it...

  abstract class CSMatcher (needle :CharSequence) extends Matcher {

    def search (haystack :Array[Char], begin :Int, end :Int) :Int = {
      val searchEnd = end-needle.length+1
      if (searchEnd < begin) -1 else search(needle, haystack, begin, searchEnd, 1)
    }

    def searchBackward (haystack :Array[Char], begin :Int, end :Int, from :Int) :Int = {
      val start = math.min(from, end-needle.length)
      if (start < begin) -1 else search(needle, haystack, start, begin-1, -1)
    }

    def matches (haystack :Array[Char], hfirst :Int, hlast :Int) :Boolean =
      if (hfirst + needle.length > hlast) false
      else check(needle.length, needle, 0, haystack, hfirst)

    def matchLength = needle.length

    override def toString = needle.toString

    private def search (n :CharSequence, hay :Array[Char], start :Int, stop :Int, dd :Int) = try {
      val length = n.length
      var ss = start ; while (ss != stop && !check(length, n, 0, hay, ss)) ss += dd
      if (ss == stop) -1 else ss
    } catch {
      case e :ArrayIndexOutOfBoundsException =>
        println(s"Gack! [m=$this, hay=${hay.length}, start=$start, stop=$stop, dd=$dd] $e") ; -1
    }

    @tailrec
    private def check (ll :Int, n :CharSequence, nn :Int, h :Array[Char], hh :Int) :Boolean =
      (nn == ll) || (eq(n.charAt(nn), h(hh)) && check(ll, n, nn+1, h, hh+1))

    /** Compares two characters for equality. Allows for case-sensitive or insensitive matching. */
    protected def eq (have :Char, want :Char) :Boolean
  }
}
