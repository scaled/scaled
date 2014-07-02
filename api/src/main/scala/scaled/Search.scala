//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Promise}
import scala.annotation.tailrec

/** Encapsulates a search on a buffer. This is tailored to support i-search, but is exposed as a
  * reusable API in case of broader utility.
  */
abstract class Search (val min :Loc) {

  /** Finds the next occurance of the sought text, starting from `from`. If `max` is reached before
    * the text is found, `Loc.None` is returned.
    */
  def findForward (from :Loc) :Loc

  /** Finds the previous occurance of the sought text, starting from `from`. If `min` is reached
    * before the text is found, `Loc.None` is returned.
    */
  def findBackward (from :Loc) :Loc

  /** Returns the end of a match that starts at `loc`. */
  def matchEnd (loc :Loc) :Loc

  /** Replaces a match at `loc` with `lines`.
    * @param buffer a mutable reference to the buffer used for this search.
    * @return the location immediately following the replaced match. */
  def replace (buffer :Buffer, loc :Loc, lines :Seq[LineV]) :Loc

  /** Finds all occurrances of the sought text between `min` and `max`. */
  def findAll () :Seq[Loc] = {
    val matches = Seq.newBuilder[Loc]
    @tailrec @inline def loop (next :Loc) :Seq[Loc] = findForward(next) match {
      case Loc.None => matches.result
      case loc      => matches += loc ; loop(matchEnd(loc))
    }
    loop(min)
  }

  /** Returns a string describing this search (for display to user). */
  def show :String
}

/** Search constructors. */
object Search {

  /** Creates a [[Search]] with the specified parameters. */
  def apply (buffer :BufferV, min :Loc, max :Loc, sought :Matcher) :Search = new Search(min) {
    override def findForward (from :Loc) = buffer.findForward(sought, from, max)
    override def findBackward (from :Loc) = buffer.findBackward(sought, from, min)
    override def matchEnd (loc :Loc) = loc + (0, sought.matchLength)
    override def replace (buffer :Buffer, loc :Loc, lines :Seq[LineV]) =
      sought.replace(buffer, loc, lines)
    override def show = sought.show
  }

  /** Creates a [[Search]] with the specified parameters. */
  def apply (buffer :BufferV, min :Loc, max :Loc, sought :LineV) :Search =
    if (sought.length == 0) NilSearch
    else apply(buffer, min, max, Matcher.on(sought))

  /** Creates a [[Search]] with the specified parameters. */
  def apply (buffer :BufferV, min :Loc, max :Loc, sought :Seq[LineV]) :Search = sought match {
    case Seq() => NilSearch
    case Seq(ln) => apply(buffer, min, max, ln)
    case lines => throw new UnsupportedOperationException("Multiline searches not yet supported.")
  }

  private val NilSearch = new Search(Loc.Zero) {
    override def findForward (from :Loc) = Loc.None
    override def findBackward (from :Loc) = Loc.None
    override def matchEnd (loc :Loc) = loc
    override def replace (buffer :Buffer, loc :Loc, lines :Seq[LineV]) = loc
    override def show = ""
  }
}
