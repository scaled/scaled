//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.SignalV

/** Provides the read-only API to a single line of text in a buffer. This is extended by [[Line]]
  * which provides a means to mutate lines. */
abstract class LineV {

  /** The length (in characters) of this line. */
  def length :Int

  /** Returns the character at `pos`. If `pos` is >= [[length]] line 0 is returned. */
  def charAt (pos :Int) :Char

  /** Bounds the supplied column into this line. This adjusts it to be in [0, [[length]]] (inclusive
    * of the length because the point can be after the last char on this line). */
  def bound (col :Int) :Int = math.max(0, math.min(length, col))

  /** Bounds the supplied loc into this line by bounding its column via [[bound(Int)]]. */
  def bound (loc :Loc) :Loc = loc.atCol(bound(loc.col))

  /** Extracts the specified region of this line into a new line.
    * @param start the index of the first character to include in the slice.
    * @param until one past the index of the last character to include in the slice. */
  def slice (start :Int, until :Int = length) :Line

  /** Extracts the slice of characters beginning at `start` and ending just before `until`. */
  def sliceChars (start :Int, until :Int = length) :Array[Char]

  /** Returns `sliceChars(start, until)` as a String (more efficiently). */
  def sliceString (start :Int, until :Int = length) :String

  /** Returns the contents of this line as a string. */
  def asString :String = sliceString(0, length)
}

/** Provides the read-write API for a single line of text in a buffer. */
abstract class Line extends LineV {

  /** Returns this line's index in the buffer that contains it. Note: this is O(N). */
  def index :Int

  /** Inserts the single character `c` into this line at `pos`. */
  def insert (pos :Int, c :Char) :Unit
  /** Inserts the characters `cs` into this line at `pos`. */
  def insert (pos :Int, cs :Array[Char]) :Unit = insert(pos, cs, 0, cs.length)
  /** Inserts the characters `cs` starting at `offset` and extending for `count` characters into this
    * line at `pos`. */
  def insert (pos :Int, cs :Array[Char], offset :Int, count :Int) :Unit

  /** Inserts the contents of `line` into this line at `pos`. */
  def insert (pos :Int, line :LineV) :Unit = insert(pos, line, 0, line.length)
  /** Inserts the contents of `line` into this line at `pos`. */
  def insert (pos :Int, line :LineV, offset :Int, count :Int) :Unit

  /** Inserts the string `str` into this line at `pos`. */
  def insert (pos :Int, str :String) {
    if (str.length == 1) insert(pos, str.charAt(0))
    else insert(pos, str.toCharArray)
  }

  /** Appends `c` to this line. */
  def append (c :Char) = insert(length, c)
  /** Appends `cs` to this line. */
  def append (cs :Array[Char]) = insert(length, cs)
  /** Appends `line` to this line. */
  def append (line :LineV) :Unit = insert(length, line)

  /** Prepends `c` to this line. */
  def prepend (c :Char) = insert(0, c)
  /** Prepends `cs` to this line. */
  def prepend (cs :Array[Char]) = insert(0, cs)
  /** Prepends `line` to this line. */
  def prepend (line :LineV) :Unit = insert(0, line)

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
    /** The characters that were deleted. */
    deletedChars :Array[Char],
    /** The number of characters that were added. */
    added :Int,
    /** The line that was edited. */
    line :Line) extends Undoable {

    /** Returns the `idx`th added character. */
    def addedChar (idx :Int) = line.charAt(offset+idx)
    /** Extracts and returns the characters that were inserted. */
    def addedChars :Array[Char] = line.sliceChars(offset, offset+added)
    /** Extracts and returns, as a string, the characters that were inserted. */
    def addedString :String = line.sliceString(offset, offset+added)
    /** The number of characters that were deleted. */
    def deleted = deletedChars.length

    // remove the added characters and add the removed characters
    override def undo () = line.replace(offset, added, deletedChars)

    override def toString = s"LEdit[${line.index}@$offset " +
                            s"-'${deletedChars.mkString}'/$deleted " +
                            s"+'${addedChars.mkString}'/$added]"
  }
}

/** The reactive version of [Line]. */
abstract class RLine extends Line {

  /** A signal dispatched when this line is edited. */
  def edited :SignalV[Line.Edit]
}
