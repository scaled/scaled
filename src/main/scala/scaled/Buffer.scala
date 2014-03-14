//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

import reactual.{SignalV, ValueV}

import scala.annotation.tailrec

/** A location in a buffer. This is a very ephemeral class. Any change to its associated buffer
  * will result in the locations offsets becoming invalid. */
case class Loc (
  /** The index of the line on which this location resides, aka the current row. */
  row :Int,
  /** The character offset into the line, aka the current column. */
  col :Int) {

  /** Returns true if this loc is earlier in the buffer than `other` (i.e. less than it).
    * Naturally both locs must refer to the same buffer. */
  def < (other :Loc) = (row < other.row) || (row == other.row && col < other.col)

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
}

/** [[Loc]] related utilities. */
object Loc {
  val Zero = Loc(0, 0)
}

/** A location in a buffer which responds as predictably as possible to changes in the buffer.
  * Edits that precede the anchor cause it to shift forward or back appropriately. Edits after the
  * anchor do not cause movement. Deleting the text that includes the anchor causes it to move to
  * the start of the deleted range.
  */
trait Anchor {

  /** Returns this anchor's current location. */
  def loc :Loc
}

/** Manages a sequence of characters, providing a line-by-line view and the means to translate
  * between character offset and line offset plus (intra-line) character offset. This isolates the
  * read-only API, see [[Buffer]] for the mutable API and [[RBuffer]] for the reactive-mutable API.
  *
  * Note: there's only one implementation of buffers, but the API factorization exists to allow
  * differing degrees of control to be exposed depending on the circumstances. If someone should
  * not be allowed to mutate a buffer, they can be supplied with a `BufferV` reference. If they can
  * mutate, but should not be allowed to register (possibly memory leak-inducing) reactive
  * handlers, they can be supplied with a `Buffer` reference. If they should have all the games and
  * all the puzzles, they get an `RBuffer` reference.
  *
  * For better and worse, the JVM allows you to downcast to the more powerful type. That's a bad
  * idea. Don't do it. Consider yourself warned.
  */
abstract class BufferV {

  /** The name of this buffer. Tends to be the name of the file from which it was read, but may
    * differ if two buffers exist with the same file name. */
  def name :String

  /** The file being edited by this buffer. */
  def file :File

  /** The current mark, if any. */
  def mark :Option[Loc]

  /** Whether or not this buffer has been modified since it was loaded or last saved. */
  def dirty :Boolean

  /** Returns the position at the start of the buffer. This is always [[Loc.Zero]], but this method
    * exists for symmetry with [[end]]. */
  def start :Loc = Loc.Zero

  /** Returns the position at the end of the buffer. This will be one character past the end of the
    * last line in the buffer. */
  def end :Loc = Loc(lines.size-1, lines.last.length)

  /** Returns a location for the specified character offset into the buffer. If `offset` is greater
    * than the length of the buffer, the returned `Loc` will be positioned after the buffer's final
    * character. */
  def loc (offset :Int) :Loc

  /** Returns the character offset into the buffer of `loc`. */
  def offset (loc :Loc) :Int

  /** A read-only view of the lines in this buffer. */
  def lines :Seq[LineV]

  /** Returns the `idx`th line. Indices are zero based.
    * @throws IndexOutOfBoundsException if `idx` is not a valid line index. */
  def line (idx :Int) :LineV = lines(idx)

  /** Returns the line referenced by `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def line (loc :Loc) :LineV = line(loc.row)

  /** Returns the length of the line at `idx`, or zero if `idx` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (idx :Int) :Int = if (idx < 0 || idx >= lines.length) 0 else lines(idx).length

  /** Returns the length of the line at `loc`, or zero if `loc` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (loc :Loc) :Int = lineLength(loc.row)

  /** Returns the character at `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def charAt (loc :Loc) :Char = line(loc.row).charAt(loc.col)

  /** Returns the data between `[start, until)` as a sequence of lines. Note: the last line does not
    * conceptually include a trailing newline, and [[insert(Region)]] takes this into account. */
  def region (start :Loc, until :Loc) :Seq[Line]

  /** Returns the start of the line at `row`. */
  def lineStart (row :Int) :Loc = Loc(row, 0)

  /** Returns the start of the line at `loc.row`. */
  def lineStart (loc :Loc) :Loc = loc.atCol(0)

  /** Returns the end of the line at `row`. */
  def lineEnd (row :Int) :Loc = Loc(row, lineLength(row))

  /** Returns the end of the line at `loc.row`. */
  def lineEnd (loc :Loc) :Loc = loc.atCol(lineLength(loc.row))

  /** Bounds `loc` into this buffer. Its row will be bound to [0, `lines.length`) and its column
    * bound into the line to which its row was bound. */
  def bound (loc :Loc) :Loc = {
    if (loc.row >= lines.size) loc.at(lines.size-1, lines.last.length)
    else if (loc.row < 0) Loc(0, lines(0).bound(loc.col))
    else lines(loc.row).bound(loc)
  }

  /** Returns the loc `count` characters forward of `loc`, or [[end]] if we reach it first. */
  def forward (loc :Loc, count :Int) :Loc = {
    @tailrec def seek (row :Int, col :Int, remain :Int) :Loc = {
      val lcol = col + remain
      val llen = lines(row).length
      if (llen >= lcol) Loc(row, lcol)
      else if (row == lines.length-1) end
      else seek(row+1, 0, lcol-llen-1) // -1 to account for line separator
    }
    seek(loc.row, loc.col, count)
  }

  /** Returns the loc `count` characters backward of `loc`, or [[start]] if we reach it first. */
  def backward (loc :Loc, count :Int) :Loc = {
    @tailrec def seek (row :Int, col :Int, remain :Int) :Loc = {
      val lcol = col - remain
      if (lcol >= 0) Loc(row, lcol)
      else if (row == 0) start
      else seek(row-1, line(row-1).length, remain-col-1) // -1 to account for line separator
    }
    seek(loc.row, loc.col, count)
  }
}

/** Extends [[BufferV]] with a mutation API. See [[RBuffer]] for a further extension which provides
  * the ability to register to react to changes.
  */
abstract class Buffer extends BufferV {

  /** Sets the current mark to `loc`. The mark will be [[bound]] into the buffer. */
  def mark_= (loc :Loc) :Unit
  /** Clears the current mark. */
  def clearMark () :Unit

  /** That which handles undoing and redoing for this buffer. */
  def undoer :Undoer

  /** Inserts the single character `c` into this buffer at `loc`. */
  def insert (loc :Loc, c :Char) :Unit

  /** Inserts the raw string `s` into this buffer at `loc`. */
  def insert (loc :Loc, s :String) {
    if (s.length == 1) insert(loc, s.charAt(0))
    else insert(loc, new Line(s))
  }

  /** Inserts the contents of `line` into this buffer at `loc`. The line in question will be spliced
    * into the line at `loc`, a new line will not be created. If you wish to create a new line,
    * [[split]] at `loc` and then insert into the appropriate half.
    *
    * @return the buffer location just after the inserted line. */
  def insert (loc :Loc, line :LineV) :Loc

  /** Inserts `region` into this buffer at `loc`. `region` will often have come from a call to
    * [[region]] or [[delete(Loc,Loc)]].
    *
    * The lines will be spliced into the line at `loc` which is almost certainly what you want.
    * This means the line at `loc` will be [[split]] in two, the first line in `region` will be
    * appended to the first half of the split line, and the last line `region` will be prepended to
    * the last half of the split line; the lines in between (if any) are inserted as is. If
    * `region` is length 1 this has the same effect as [[insert(Loc,Line)]].
    *
    * @return the buffer location just after the end of the inserted region.
    */
  def insert (loc :Loc, region :Seq[LineV]) :Loc

  /** Deletes `count` characters from the line at `loc`.
    * @return the deleted chars as a line. */
  def delete (loc :Loc, count :Int) :Line

  /** Deletes the data between `[start, until)` from the buffer. Returns a copy of the deleted data.
    * Note: the last line does not conceptually include a trailing newline, and [[insert(Region)]]
    * takes this into account. */
  def delete (start :Loc, until :Loc) :Seq[Line]

  /** Replaces `delete` characters in the line at `loc` with the `line`.
    * @return the replaced characters as a line. */
  def replace (loc :Loc, delete :Int, line :Line) :Line

  /** Replaces the region between `[start, until)` with `lines`.
    * @return the buffer location just after the replaced region. */
  def replace (start :Loc, until :Loc, lines :Seq[Line]) :Loc = {
    if (until < start) replace(until, start, lines) else {
      delete(start, until)
      insert(start, lines)
    }
  }

  /** Splits the line at `loc`. The characters up to `loc.col` will remain on the `loc.row`th line,
    * and the character at `loc.col` and all subsequent characters will be moved to a new line
    * which immediately follows the `loc.row`th line. */
  def split (loc :Loc) :Unit

  private[scaled] def undo (edit :Buffer.Edit) :Unit

  // TODO: methods for editing based on a pair of Locs
}

/** `Buffer` related types and utilities. */
object Buffer {

  /** An event emitted when one or more lines are deleted from a buffer and replaced by one or more
    * new lines. The removed lines will have already been removed and the added lines added when
    * this edit is dispatched. */
  case class Edit (
    /** The offset (zero-based line number) in the buffer at which lines were replaced. */
    offset :Int,
    /** The lines that were deleted. */
    deletedLines :Seq[Line],
    /** The number of lines that were added. */
    added :Int,
    /** The buffer that was edited. */
    buffer :Buffer) extends Undoable {

    /** Extracts and returns the lines that were added. */
    def addedLines :Seq[LineV] = buffer.lines.slice(offset, offset+added)
    /** The number of lines that were deleted. */
    def deleted :Int = deletedLines.size

    /** Undoes this edit. */
    override def undo () :Unit = buffer.undo(this)

    override def toString = s"BEdit[@$offset, -${deletedLines.size} +$added"
  }
}

/** The reactive version of [Buffer]. */
abstract class RBuffer extends Buffer {

  /** A reactive view of [[name]]. */
  def nameV :ValueV[String]

  /** A reactive view of [[file]]. */
  def fileV :ValueV[File]

  /** The current mark, if any. */
  def markV :ValueV[Option[Loc]]

  /** A reactive view of [[dirty]]. */
  def dirtyV :ValueV[Boolean]

  /** A signal emitted when this buffer is edited. */
  def edited :SignalV[Buffer.Edit]

  /** A signal emitted when any of this buffer's lines are edited. */
  def lineEdited :SignalV[Line.Edit]

  // implement some Buffer methods in terms of our reactive values
  override def name = nameV.get
  override def file = fileV.get
  override def mark = markV.get
  override def dirty = dirtyV.get
}
