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

  /** Returns true if this loc is earlier in the buffer than `other` (i.e. less than it).
    * Naturally both locs must refer to the same buffer. */
  def < (other :Loc) = {
    val trow = row
    val orow = other.row
      (trow < orow) || (trow == orow && col < other.col)
  }

  /** Returns the lesser (earlier in the buffer) of `this` and `other`. */
  def lesser (other :Loc) = if (this < other) this else other

  /** Returns a loc adjusted by `deltaRow` rows and `deltaCol` columns. */
  def + (deltaRow :Int, deltaCol :Int) = at(row+deltaRow, col+deltaCol)
  /** Returns a loc on row `row` at this loc's column. */
  def atRow (row :Int) = at(row, col)
  /** Returns a loc on column `col` at this loc's row. */
  def atCol (col :Int) = at(row, col)
  /** Returns this `loc` if `row` and `col` are equal to its values, otherwise returns a new `Loc` at
    * the specified coordinates. Useful for bounding locations without superfluous allocation. */
  def at (row :Int, col :Int) =
    if (this.row == row && this.col == col) this else Loc(row, col)

  /** Returns the loc directly to the left of this loc. */
  def prevC = at(row, col-1)
  /** Returns the loc directly to the right of this loc. */
  def nextC = at(row, col+1)
  /** Returns the loc directly above this loc. */
  def prevL = at(row-1, col)
  /** Returns the loc directly below this loc. */
  def nextL = at(row+1, col)

  /** Returns the start of the line after the line referenced by this loc. */
  def nextStart = at(row+1, 0)

  override def toString = s"r$row:c$col"
}

/** [[Loc]] related utilities. */
object Loc {

  val Zero = Loc(0, 0)

  /** Creates a location with the specified row and column. */
  def apply (row :Int, col :Int) = new Loc((row.toLong << 32) + col)
}
