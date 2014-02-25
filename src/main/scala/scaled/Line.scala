//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.SignalV

/** Models a single line of text in a buffer. Provides means to read and update said text. */
abstract class Line {

  /** The length (in characters) of this line. */
  def length :Int

  /** Returns the character at `pos`. */
  def charAt (pos :Int) :Char

  /** Bounds the supplied column into this line. This adjusts it to be in [0, [[length]]] (inclusive
    * of the length because the point can be after the last char on this line). */
  def bound (col :Int) :Int = math.max(0, math.min(length, col))

  /** Bounds the supplied loc into this line by bounding its column via [[bound(Int)]]. */
  def bound (loc :Loc) :Loc = loc.atCol(bound(loc.col))

  /** Extracts the slice of characters beginning at `start` and ending just before `until`. */
  def slice (start :Int, until :Int) :Array[Char]

  /** Returns `slice(start, until)` as a String (more efficiently). */
  def sliceString (start :Int, until :Int) :String

  /** Inserts the single character `c` into this line at `pos`. */
  def insert (pos :Int, c :Char) :Unit

  /** Inserts the characters `cs` into this line at `pos`. */
  def insert (pos :Int, cs :Array[Char]) :Unit = insert(pos, cs, 0, cs.length)

  /** Inserts the characters `cs` starting at `offset` and extending for `count` characters into this
    * line at `pos`. */
  def insert (pos :Int, cs :Array[Char], offset :Int, count :Int) :Unit

  /** Inserts the string `str` into this line at `pos`. */
  def insert (pos :Int, str :String) {
    if (str.length == 1) insert(pos, str.charAt(0))
    else insert(pos, str.toCharArray)
  }

  /** Deletes `length` characters starting at `pos`. */
  def delete (pos :Int, length :Int) :Unit

  /** Replaces `delete` characters starting at `pos` with the characters in `cs`. */
  def replace (pos :Int, delete :Int, cs :Array[Char]) :Unit
}

/** `Line` related types and utilities. */
object Line {

  /** An event emitted when one or more characters are deleted from a line and replaced by one or
    * more characters. The removed characters will have already been removed and the added
    * characters added when this edit is dispatched. */
  case class Edit (
    /** The offset into `line.chars` at which the characters were replaced. */
    offset :Int,
    /** The number of characters that were deleted. */
    deleted :Int,
    /** The number of characters that were added. */
    added :Int,
    /** The line that was edited. */
    line :Line) {
    /** Extracts and returns the characters that were inserted. */
    def addedChars :Array[Char] = line.slice(offset, offset+added)
    /** Extracts and returns, as a string, the characters that were inserted. */
    def addedString :String = line.sliceString(offset, offset+added)
  }
}

/** The reactive version of [Line]. */
abstract class RLine extends Line {

  /** A signal dispatched when this line is edited. */
  def edited :SignalV[Line.Edit]
}
