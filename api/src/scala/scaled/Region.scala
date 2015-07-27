//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Denotes a region of a buffer. */
trait Region {

  /** The location at which the region starts. */
  def start :Loc

  /** The location immediately following the last location in the region. */
  def end :Loc

  /** Returns true if this region (`[start,end)`) contains `loc`. */
  def contains (loc :Loc) :Boolean = (start <= loc) && (loc < end)

  /** Returns true if this region is empty (`start >= end`). */
  def isEmpty = start >= end
}

object Region {

  /** An empty region from zero to zero. */
  val Empty = apply(Loc.Zero, Loc.Zero)

  /** Creates a region that spans `[start:end)`. */
  def apply (start :Loc, end :Loc) :Region = new Simple(start, end)
  private case class Simple (start :Loc, end :Loc) extends Region {
    override def toString = Region.toString(this)
  }

  /** Unapplies region `r`, for pattern matching. */
  def unapply (r :Region) = Some((r.start, r.end))

  /** Formats the region `r`. Single line regions as `rR:cA-B`, multi-line regions as
    * `rR:cC-rS:cD`. */
  def toString (r :Region) :String = toString(r.start, r.end)

  /** Formats the region identified by `start` and `end`. Single line regions as `rR:cA-B`,
    * multi-line regions as `rR:cC-rS:cD`. */
  def toString (start :Loc, end :Loc) :String =
    if (start.row == end.row) s"r${start.row}:c${start.col}-${end.col}"
    else s"$start-$end"

  /** Finds the region in the supplied ordered sequence of non-overlapping regions which contains
    * `loc`, if any. A binary search is used to search regions. */
  def find[R <: Region] (rs :SeqV[R], loc :Loc) :Option[R] = {
    var low = 0 ; var high = rs.size-1
    while (low <= high) {
      val mid = (low + high) >>> 1
      val midR = rs(mid)
      val midS = midR.start
      if (midS < loc) low = mid + 1
      else if (midS > loc) high = mid - 1
      else return Some(midR)
    }
    if (high < 0) None
    else {
      val highR = rs(high)
      if (loc < highR.end) Some(highR) else None
    }
  }
}
