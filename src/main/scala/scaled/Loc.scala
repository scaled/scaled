//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

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

  /** Returns true if this loc is earlier than `other` (i.e. less than it). */
  def < (other :Loc) :Boolean = {
    val trow = row ; val orow = other.row
    (trow < orow) || (trow == orow && col < other.col)
  }
  /** Returns true if this loc is later than `other` (i.e. greather than it). */
  def > (other :Loc) :Boolean = other < this

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

  val Zero = Loc(0, 0)

  /** Creates a location with the specified row and column. */
  def apply (row :Int, col :Int) = new Loc((row.toLong << 32) + col)
}
