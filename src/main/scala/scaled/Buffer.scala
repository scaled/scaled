//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

import reactual.{SignalV, ValueV}

/** A location in a buffer. This is a very ephemeral class. Any change to its associated buffer
  * will result in the locations offsets becoming invalid. */
case class Loc (
  /** The index of the line on which this location resides, aka the current row. */
  row :Int,
  /** The character offset into the line, aka the current column. */
  col :Int) {

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
  * between character offset and line offset plus (intra-line) character offset.
  */
abstract class Buffer {

  /** The name of this buffer. Tends to be the name of the file from which it was read. */
  def name :String

  /** The directory from which this buffer was loaded, or to which it was most recently saved. */
  def dir :File

  /** A read-only view of the lines in this buffer. */
  def lines :Seq[Line]

  /** Returns the `idx`th line. Indices are zero based.
    * @throws ArrayIndexOutOfBoundsException if `idx` is not a valid line index. */
  def line (idx :Int) :Line = lines(idx)

  /** Returns the line referenced by `loc`.
    * @throws ArrayIndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def line (loc :Loc) :Line = line(loc.row)

  /** Returns the character at `loc`.
    * @throws ArrayIndexOutOfBoundsException if `loc.row` is not a valid line index or `loc.col`
    * is not a valid character index in that line. */
  def charAt (loc :Loc) :Char = line(loc.row).charAt(loc.col)

  /** Returns the length of the line at `idx`, or zero if `idx` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (idx :Int) :Int = if (idx < 0 || idx >= lines.length) 0 else lines(idx).length

  /** Returns the length of the line at `loc`, or zero if `loc` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (loc :Loc) :Int = lineLength(loc.row)

  /** Returns the loc `count` characters forward of `loc`. */
  def forward (loc :Loc, count :Int) :Loc = this.loc(offset(loc)+count) // TODO: optimize

  /** Returns the loc `count` characters backward of `loc`. */
  def backward (loc :Loc, count :Int) :Loc = this.loc(offset(loc)-count) // TODO: optimize

  /** Returns a location for the specified character offset into the buffer. If `offset` is greater
    * than the length of the buffer, the returned `Loc` will be positioned after the buffer's final
    * character. */
  def loc (offset :Int) :Loc

  /** Returns the character offset into the buffer of `loc`. */
  def offset (loc :Loc) :Int

  /** TEMP: Returns the "word" at the specified location. */
  def wordAt (loc :Loc) :String = "TEMP"

  /** Inserts a single line at index `idx` with `text` as its contents. Note: `text` should not
    * contain newlines, the caller should split such text into separate lines and use the
    * multi-line `insert`. */
  def insert (idx :Int, text :Array[Char]) :Unit = insert(idx, Array(text))

  /** Inserts multiple lines at index `idx`. */
  def insert (idx :Int, lines :Seq[Array[Char]]) :Unit

  /** Deletes `count` lines starting at `idx`. */
  def delete (idx :Int, count :Int) :Unit

  /** Splits the line at `loc`. The characters up to `loc.col` will remain on the `loc.row`th line,
    * and the character at `loc.col` and all subsequent characters will be moved to a new line
    * which immediately follows the `loc.row`th line. */
  def split (loc :Loc) :Unit = split(loc.row, loc.col)

  /** Splits the `idx`th line at character position `pos`. The characters up to `pos` will remain on
    * the `idx`th line, and the character at `pos` and all subsequent characters will be moved to a
    * new line which immediately follows the `idx`th line. */
  def split (idx :Int, pos :Int) :Unit

  /** Joins the `idx`th line with the line immediately following it. */
  def join (idx :Int) :Unit

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
    def addedLines :Seq[Line] = buffer.lines.slice(offset, offset+added)
    /** The number of lines that were deleted. */
    def deleted :Int = deletedLines.size

    /** Undoes this edit. */
    override def undo () :Unit = buffer.undo(this)

    override def toString = s"BEdit[@$offset, -${deletedLines.size} +$added"
  }
}

/** The reactive version of [Buffer]. */
abstract class RBuffer extends Buffer {

  /** A reactive view of [name]. */
  def nameV :ValueV[String]

  /** A reactive view of [dir]. */
  def dirV :ValueV[File]

  /** A signal emitted when this buffer is edited. */
  def edited :SignalV[Buffer.Edit]

  /** A signal emitted when any of this buffer's lines are edited. */
  def lineEdited :SignalV[Line.Edit]
}
