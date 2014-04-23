//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import scala.annotation.tailrec

/** Handles searching and matching in [[Line]]s. */
abstract class Matcher {

  /** Searches forward for a match in `[begin,end)` of `haystack` starting at `begin`.
    * @return the index at which a match occurred or -1. */
  def search (haystack :Array[Char], begin :Int, end :Int) :Int

  /** Searches backward for a match in `[begin,end)` of `haystack` starting at `from`.
    * @return the index at which a match occurred or -1. */
  def searchBackward (haystack :Array[Char], begin :Int, end :Int, from :Int) :Int

  /** Returns true if this matcher matches `haystack` starting at `begin` without extending to or
    * beyond `end`. */
  def matches (haystack :Array[Char], begin :Int, end :Int) :Boolean

  /** Returns the length of the most recent match. This method must be preceeded by a call to
    * [[search]], [[searchBackward]] or [[matches]] which returned a successful match, otherwise
    * its return value is undefined. */
  def matchLength :Int
}

object Matcher {

  /** Returns a case-sensitive matcher on `cs`. */
  def exact (cs :CharSequence) :Matcher = new CSMatcher(cs) {
    protected def eq (have :Char, want :Char) = have == want
  }

  /** Returns a case-insensitive matcher on `cs`. NOTE: `cs` must be all lower case. */
  def loose (cs :CharSequence) :Matcher = new CSMatcher(cs) {
    protected def eq (have :Char, want :Char) = have == want || have == Character.toLowerCase(want)
  }

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
    else if (Character.isUpperCase(cs.charAt(ii))) false
    else mixedCase(cs, ii+1)

  // TODO: if it turns out to be worth it performance-wise, we can create specialized matchers on
  // Line which operate directly on the _chars array; it's probably not worth it...

  abstract class CSMatcher (needle :CharSequence) extends Matcher {

    def search (haystack :Array[Char], begin :Int, end :Int) :Int =
      if (end < begin) -1 else search(needle, haystack, begin, end, 1)

    def searchBackward (haystack :Array[Char], begin :Int, end :Int, from :Int) :Int = {
      val start = math.min(from, end-needle.length)
      if (start < begin) -1 else search(needle, haystack, start, begin-1, -1)
    }

    def matches (haystack :Array[Char], hfirst :Int, hlast :Int) :Boolean =
      if (hfirst + needle.length > hlast) false
      else check(needle.length, needle, 0, haystack, hfirst)

    def matchLength = needle.length

    override def toString = needle.toString

    private def search (n :CharSequence, hay :Array[Char], start :Int, stop :Int, dd :Int) = {
      val length = n.length
      var ss = start ; while (ss != stop && !check(length, n, 0, hay, ss)) ss += dd
      if (ss == stop) -1 else ss
    }

    @tailrec
    private def check (ll :Int, n :CharSequence, nn :Int, h :Array[Char], hh :Int) :Boolean =
      (nn == ll) || (eq(n.charAt(nn), h(hh)) && check(ll, n, nn+1, h, hh+1))

    /** Compares two characters for equality. Allows for case-sensitive or insensitive matching. */
    protected def eq (have :Char, want :Char) :Boolean
  }
}
