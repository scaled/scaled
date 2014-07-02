//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import reactual.{Future, Promise}

/** Encapsulates a search on a buffer. This is tailored to support i-search, but is exposed as a
  * reusable API in case of broader utility.
  */
abstract class Search (min :Loc) {

  /** Finds the next occurance of the sought text, starting from `from`. If `max` is reached before
    * the text is found, `Loc.None` is returned.
    */
  def findForward (from :Loc) :Loc

  /** Finds the previous occurance of the sought text, starting from `from`. If `min` is reached
    * before the text is found, `Loc.None` is returned.
    */
  def findBackward (from :Loc) :Loc

  /** Finds all occurrances of the sought text between `min` and `max`. */
  def findAll () :Seq[Loc] = {
    val matches = Seq.newBuilder[Loc]
    var next = min
    while (true) findForward(next) match {
      case Loc.None => return matches.result
      case loc      => matches += loc ; next = advance(loc)
    }
    Seq() // not reached, but the compiler don't know dat
  }

  /** Advances `loc` (which is a match) to the position at which the next search should start. */
  protected def advance (loc :Loc) :Loc
}

/** Search constructors. */
object Search {

  /** Creates a [[Search]] with the specified parameters. */
  def apply (buffer :BufferV, min :Loc, max :Loc, sought :LineV) :Search =
    if (sought.length == 0) NilSearch
    else return new Search(min) {
      private val m = Matcher.on(sought)
      def findForward (from :Loc) = buffer.findForward(m, from, max)
      def findBackward (from :Loc) = buffer.findBackward(m, from, min)
      protected def advance (loc :Loc) = loc + (0, sought.length)
    }

  /** Creates a [[Search]] with the specified parameters. */
  def apply (buffer :BufferV, min :Loc, max :Loc, sought :Seq[LineV]) :Search = sought match {
    case Seq() => NilSearch
    case Seq(ln) => apply(buffer, min, max, ln)
    case lines => throw new UnsupportedOperationException("Multiline searches not yet supported.")
  }

  private val NilSearch = new Search(Loc.Zero) {
    override def findForward (from :Loc) = Loc.None
    override def findBackward (from :Loc) = Loc.None
    override protected def advance (loc :Loc) = loc
  }
}
