//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import reactual.{Signal, SignalV}

import scaled._

/** [MutableLine] related types and utilities. */
object MutableLine {

  /** The number of extra characters padded onto an expanded line. */
  final val ExpandN = 32

  /** An empty character array used for edits that delete no characters. */
  final val NoChars = Array[Char]()
}

/** [LineV] with mutable internals so that `BufferImpl` can efficiently edit it.
  *
  * Note: most of the mutation methods take a [[Loc]] instead of an `Int` to indicate the offset
  * into the line at which the edit is to take place. This loc contains the to-be-edited line's
  * current `row` in addition to the line offset (`col`). This is because edits must emit an edited
  * event which contains the row at which the line in question resides. The buffer knows the line's
  * current row at the time that it makes the edit, so it passes that information in, along with
  * the line position at which the edit is to take place. This also often allows the `Loc` that
  * initiated an edit to be reused all the way through, without further allocation. We also prefer
  * that the line emit this event rather than the buffer, so that the caller can't "forget" to emit
  * an event along with a line edit.
  *
  * @param initChars The initial characters in this line. Ownership of this array is taken by this
  * line instance and the array may subsequently be mutated thereby.
  */
class MutableLine (buffer :BufferImpl, initChars :Array[Char]) extends LineV {

  /** Creates a mutable line with a copy of the contents of `line`. */
  def this (buffer :BufferImpl, line :LineV) = this(buffer, {
    val cs = Array.ofDim[Char](line.length)
    line.sliceInto(0, line.length, cs, 0)
    cs
  })

  private var _chars = initChars
  private var _end = initChars.size

  override def length = _end
  override def charAt (pos :Int) = if (pos < _end) _chars(pos) else 0
  override def view (start :Int, until :Int) = new Line(_chars, start, until-start)
  override def slice (start :Int, until :Int) = new Line(_chars.slice(start, until))
  override def sliceInto (start :Int, until :Int, cs :Array[Char], offset :Int) {
    System.arraycopy(_chars, start, cs, offset, until-start)
  }
  override def asString :String = new String(_chars, 0, _end)

  /** The array that contains this line's characters. It's size may exceed `length` for reasons of
    * efficiency. Be sure to use [length], not `chars.length`. */
  def chars :Array[Char] = _chars

  /** Splits this line at `loc`. Deletes the data from `loc.col` onward from this line.
    * @return a new line which contains the data from `loc.col` onward. */
  def split (loc :Loc) :MutableLine = {
    // TODO: if loc.col is close to zero, just give our internals to the new line and create new
    // internals for ourselves?
    new MutableLine(buffer, delete(loc, _end-loc.col))
  }

  /** Inserts `c` into this line at `loc`. */
  def insert (loc :Loc, c :Char) {
    prepInsert(loc.col, 1)
    _chars(loc.col) = c
    _end += 1
    buffer.noteLineEdited(loc, Line.Empty, 1)
  }

  /** Inserts `[offset, offset+count)` slice of `line` into this line at `pos` <em>without emitting
    * an edited event</em>. */
  def insertSilent (pos :Int, line :LineV, offset :Int, count :Int) {
    prepInsert(pos, count)
    line.sliceInto(offset, offset+count, _chars, pos)
    _end += count
  }

  /** Inserts `[offset, offset+count)` slice of `line` into this line at `loc`. */
  def insert (loc :Loc, line :LineV, offset :Int, count :Int) :Loc = {
    insertSilent(loc.col, line, offset, count)
    buffer.noteLineEdited(loc, Line.Empty, count)
    loc + (0, count)
  }

  /** Inserts `line` into this line at `loc`. */
  def insert (loc :Loc, line :LineV) :Loc = insert(loc, line, 0, line.length)

  /** Appends `line` to this line. */
  def append (loc :Loc, line :LineV) :Unit = insert(loc.atCol(_end), line)

  /** Deletes `length` chars from this line starting at `loc`.
    * @return the deleted chars as a line. */
  def delete (loc :Loc, length :Int) :Line = {
    val pos = loc.col
    val last = pos + length
    assert(pos >= 0 && last <= _end)
    val deleted = slice(pos, pos+length)
    System.arraycopy(_chars, last, _chars, pos, _end-last)
    _end -= length
    buffer.noteLineEdited(loc, deleted, 0)
    deleted
  }

  /** Replaces `delete` chars starting at `loc` with `line`. */
  def replace (loc :Loc, delete :Int, line :LineV) :Line = {
    val pos = loc.col
    val lastDeleted = pos + delete
    assert(lastDeleted <= _end)
    val added = line.length
    val lastAdded = pos + added
    val replaced = if (delete > 0) slice(pos, pos+delete) else Line.Empty

    // if we have a net increase in characters, shift tail right to make room
    val deltaLength = lastAdded - lastDeleted
    if (deltaLength > 0) prepInsert(pos, deltaLength)
    // if we have a net decrease, shift tail left to close gap
    else if (deltaLength < 0) System.arraycopy(
      _chars, lastDeleted, _chars, lastAdded, _end-lastDeleted)
    // otherwise, we've got a perfect match, no shifting needed

    line.sliceInto(0, added, _chars, pos)
    _end += deltaLength
    buffer.noteLineEdited(loc, replaced, added)
    replaced
  }

  override def toString () = s"$asString/${_end}/${_chars.length}"

  //
  // impl details

  private def prepInsert (pos :Int, length :Int) {
    assert(pos >= 0 && pos <= _end)
    val curlen = _chars.length
    val curend = _end
    // if we need to expand our _chars array...
    if (curend + length > curlen) {
      // ...tack on an extra N characters in expectation of future expansions
      val nchars = new Array[Char](_chars.length+length + MutableLine.ExpandN)
      System.arraycopy(_chars, 0, nchars, 0, pos)
      System.arraycopy(_chars, pos, nchars, pos+length, curend-pos)
      _chars = nchars
    }
    // otherwise shift characters down, if necessary
    else if (pos < curend) {
      System.arraycopy(_chars, pos, _chars, pos+length, curend-pos)
    }
  }
}
