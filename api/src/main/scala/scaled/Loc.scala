//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** A location in a buffer. This is a very ephemeral class. Any change to its associated buffer
  * will result in the locations offsets becoming invalid.
  *
  * Note: locations with negative row are not modeled accurately (-1 becomes -2, etc.). They will
  * compare as earlier than all locations with positive row which is generally sufficient to stave
  * off insanity, but you should avoid them in general. They sometimes occur when one calls, for
  * example `prevL`, on row zero, but when that invalid location is bounded into the buffer it
  * quickly disappears.
  */
class Loc private (val rowCol :Long) extends AnyVal {

  /** The (zero-based) row of the buffer represented by this loc. */
  def row :Int = (rowCol >> 32).toInt
  /** The (zero-based) column of the buffer represented by this loc. */
  def col :Int = rowCol.toInt

  /** Returns true if this loc is earlier than `other`. */
  def < (other :Loc) :Boolean = {
    val trow = row ; val orow = other.row
    (trow < orow) || (trow == orow && col < other.col)
  }
  /** Returns true if this loc is earlier than or equal to `other`. */
  def <= (other :Loc) :Boolean = !(other < this)
  /** Returns true if this loc is later than `other`. */
  def > (other :Loc) :Boolean = other < this
  /** Returns true if this loc is later than or equal to `other`. */
  def >= (other :Loc) :Boolean = !(this < other)

  /** Returns `< 0` if `this` is `<` `other`, `0` if `this` == `other` and `> 0` otherwise. */
  def compareTo (other :Loc) :Int = {
    // overflow is not a risk, if a buffer has ~2 billion rows/cols, you have bigger problems
    val drow = row - other.row
    if (drow != 0) drow else col - other.col
  }

  /** Returns the lesser (earlier in the buffer) of `this` and `other`. */
  def lesser (other :Loc) :Loc = if (this < other) this else other
  /** Returns the greater (later in the buffer) of `this` and `other`. */
  def greater (other :Loc) :Loc = if (this > other) this else other

  /** Returns a loc adjusted by `deltaRow` rows and `deltaCol` columns. */
  def + (deltaRow :Int, deltaCol :Int) :Loc = Loc(row+deltaRow, col+deltaCol)

  /** "Adds" `region` to `this` loc. This returns the loc immediately after `region`, assuming
    * `region` exists in a buffer at `this` loc. */
  def + (region :Seq[LineV]) :Loc = region.size match {
    case 0 => this
    case 1 => Loc(row, col+region.head.length)
    case n => Loc(row + n-1, region.last.length)
  }

  /** Returns a loc on row `row` at this loc's column. */
  def atRow (row :Int) :Loc = Loc(row, col)
  /** Returns a loc on column `col` at this loc's row. */
  def atCol (col :Int) :Loc = Loc(row, col)

  /** Returns the loc directly to the left of this loc. */
  def prevC :Loc = Loc(row, col-1)
  /** Returns the loc directly to the right of this loc. */
  def nextC :Loc = Loc(row, col+1)
  /** Returns the loc directly above this loc. */
  def prevL :Loc = Loc(row-1, col)
  /** Returns the loc directly below this loc. */
  def nextL :Loc = Loc(row+1, col)

  /** Returns the start of the line after the line referenced by this loc. */
  def nextStart :Loc = Loc(row+1, 0)

  override def toString = s"r$row:c$col"
}

/** [[Loc]] related utilities. */
object Loc {

  /** The location at row zero, column zero. */
  val Zero = Loc(0, 0)

  /** A sentinel location indicating failure. No valid buffer location will ever equal this. */
  val None = Loc(-1, -1)

  /** Creates a location with the specified row and column. */
  def apply (row :Int, col :Int) = new Loc((row.toLong << 32) + col)

  /** Adjusts `loc` based on an insert that may have preceded it. */
  def adjustForInsert (loc :Loc, start :Loc, end :Loc) :Loc =
    if (start > loc) loc
    else if (start.row == loc.row) Loc(end.row, end.col + loc.col-start.col)
    else Loc(loc.row + end.row-start.row, loc.col)

  /** Adjusts `loc` based on a delete that may have preceded it. */
  def adjustForDelete (loc :Loc, start :Loc, end :Loc) :Loc =
    if (start > loc) loc
    else if (end > loc) start // deletion surrounded our loc
    else if (end.row == loc.row) Loc(start.row, start.col + loc.col-end.col)
    else Loc(loc.row + start.row-end.row, loc.col)

  /** Returns loc's row. Helper for use in non-Scala code. This would be named `row` but scalac
    * won't generate a static forwarder in that case because it has the same /name/ (though not the
    * same signature) as [[Loc.row]]. Sigh. */
  def r (l :Loc) = l.row

  /** Returns loc's col. Helper for use in non-Scala code. This would be named `col` but scalac
    * won't generate a static forwarder in that case because it has the same /name/ (though not the
    * same signature) as [[Loc.col]]. Sigh. */
  def c (l :Loc) = l.col

  /** Returns [[Loc.toString]]. Helper for use in non-Scala code. */
  def show (l :Loc) = l.toString
}
