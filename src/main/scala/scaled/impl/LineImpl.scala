//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import reactual.{Signal, SignalV}

import scaled.{Line, RLine}

/** [LineImpl] related types and utilities. */
object LineImpl {

  /** The number of extra characters padded onto an expanded line. */
  final val ExpandN = 32

  /** An empty character array used for edits that delete no characters. */
  final val NoChars = Array[Char]()
}

/** Implements [Line] and [RLine]. */
class LineImpl (
  /** The initial characters on this line. Ownership of this array is taken by this line instance and
    * the array may subsequently be mutated by the line. */
  initChars :Array[Char],
  /** The buffer that owns this line. */
  buffer :BufferImpl
) extends RLine {

  private var _chars :Array[Char] = initChars
  private var _end :Int = initChars.size
  private val _edited :Signal[Line.Edit] = Signal()

  /** The buffer that contains this line's characters. It's size may exceed `length` for reasons of
    * efficiency. Be sure to use [length], not `chars.length`. */
  def chars :Array[Char] = _chars

  /** Returns the contents of this line as a string. */
  def asString :String = new String(_chars, 0, _end)

  /** Splits this line at `pos`. Deletes the data from `pos` onward from this line.
    * @return a new line which contains the data from `pos` onward. */
  def split (pos :Int) :LineImpl = {
    // TODO: if pos is close to zero, just give our internals to the new line and create new
    // internals for ourselves?
    val rem = new LineImpl(_chars.slice(pos, _end), buffer)
    delete(pos, _end-pos)
    rem
  }

  /** Appends `line` to this line. */
  def append (line :LineImpl) {
    // TODO: append style information as well when we have it
    insert(_end, line.chars, 0, line.length)
  }

  override def toString () = s"$asString/${_end}/${_chars.length}"

  //
  // from Line and RLine API

  override def length = _end
  override def charAt (pos :Int) = _chars(pos)
  override def index = buffer.lines.indexOf(this)
  // TODO: document, handle, test boundary conditions? or just throw?
  override def slice (start :Int, until :Int) = _chars.slice(start, until)
  override def sliceString (start :Int, until :Int) = new String(_chars, start, until-start)
  override def edited = _edited

  override def insert (pos :Int, c :Char) {
    prepInsert(pos, 1)
    _chars(pos) = c
    _end += 1
    noteEdited(pos, LineImpl.NoChars, 1)
  }

  override def insert (pos :Int, cs :Array[Char], offset :Int, count :Int) {
    prepInsert(pos, count)
    System.arraycopy(cs, offset, _chars, pos, count)
    _end += count
    noteEdited(pos, LineImpl.NoChars, count)
  }

  override def delete (pos :Int, length :Int) {
    val last = pos + length
    assert(pos >= 0 && last <= _end)
    val deletedChars = _chars.slice(pos, pos+length)
    System.arraycopy(_chars, last, _chars, pos, _end-last)
    _end -= length
    noteEdited(pos, deletedChars, 0)
  }

  override def replace (pos :Int, delete :Int, cs :Array[Char]) {
    val lastDeleted = pos + delete
    assert(lastDeleted <= _end)
    val lastAdded = pos + cs.length
    val deletedChars = if (delete > 0) _chars.slice(pos, pos+delete) else LineImpl.NoChars

    // if we have a net increase in characters, shift tail right to make room
    val deltaLength = lastAdded - lastDeleted
    if (deltaLength > 0) prepInsert(pos, deltaLength)
    // if we have a net decrease, shift tail left to close gap
    else if (deltaLength < 0) System.arraycopy(
      _chars, lastDeleted, _chars, lastAdded, _end-lastDeleted)
    // otherwise, we've got a perfect match, no shifting needed

    System.arraycopy(cs, 0, _chars, pos, cs.length)
    _end += deltaLength
    noteEdited(pos, deletedChars, cs.length)
  }

  //
  // impl details

  private def prepInsert (pos :Int, length :Int) {
    assert(pos >= 0 && pos <= _end)
    val curlen = _chars.length
    val curend = _end
    // if we need to expand our _chars array...
    if (curend + length > curlen) {
      // ...tack on an extra N characters in expectation of future expansions
      val nchars = new Array[Char](_chars.length+length + LineImpl.ExpandN)
      System.arraycopy(_chars, 0, nchars, 0, pos)
      System.arraycopy(_chars, pos, nchars, pos+length, curend-pos)
      _chars = nchars
    }
    // otherwise shift characters down, if necessary
    else if (pos < curend) {
      System.arraycopy(_chars, pos, _chars, pos+length, curend-pos)
    }
  }

  private def noteEdited (offset :Int, deletedChars :Array[Char], added :Int) {
    val edit = Line.Edit(offset, deletedChars, added, this)
    _edited.emit(edit)
    buffer.noteEdited(edit)
  }
}
