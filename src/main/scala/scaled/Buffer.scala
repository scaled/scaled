//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

import reactual.{SignalV, ValueV}

/** A location in a buffer. This is a very ephemeral class. Any change to its associated buffer
  * will result in the locations offsets becoming invalid. */
case class Loc (
  /** The character offset in the buffer. */
  offset :Int,
  /** The index of the line on which this location resides. */
  lineIdx :Int,
  /** The character offset into the line. */
  lineOffset :Int)

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
trait Buffer {

  /** The name of this buffer. Tends to be the name of the file from which it was read. */
  def name :String

  /** The directory from which this buffer was loaded, or to which it was most recently saved. */
  def dir :File

  /** Returns the `idx`th line. Indices are zero based. */
  def line (idx :Int) :Line

  /** A read-only view of the lines in this buffer. */
  def lines :Seq[Line]

  // TODO: methods for inserting, removing and replacing lines
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
    /** The number of lines that were deleted. */
    deleted :Int,
    /** The number of lines that were added. */
    added :Int,
    /** The buffer that was edited. */
    buffer :Buffer) {
    /** Extracts and returns the lines that were added. */
    def addedLines :Seq[Line] = buffer.lines.slice(offset, offset+added)
  }
}

/** The reactive version of [Buffer]. */
trait RBuffer extends Buffer {

  /** A reactive view of [name]. */
  def nameV :ValueV[String]

  /** A reactive view of [dir]. */
  def dirV :ValueV[File]

  /** A signal emitted when this buffer is edited. */
  def edited :SignalV[Buffer.Edit]
}
