//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import reactual.{Signal, SignalV}

import scaled.{Line, RLine}

/** [LineImpl] related types and utilities. */
object LineImpl {

  /** The number of extra characters padded onto an expanded line. */
  private final val ExpandN = 32
}

/** Implements [Line] and [RLine]. */
class LineImpl (
  /** The initial characters on this line. Ownership of this array is taken by this line instance and
    * the array may subsequently be mutated by the line. */
  initChars :Array[Char]
) extends RLine {

  private var _chars :Array[Char] = initChars
  private var _end :Int = initChars.size
  private val _edited :Signal[Line.Edit] = Signal()

  /** The buffer that contains this line's characters. It's size may exceed `length` for reasons of
    * efficiency. Be sure to use [length], not `chars.length`. */
  def chars :Array[Char] = _chars

  /** Returns the contents of this line as a string. */
  def asString :String = new String(_chars, 0, _end)

  override def toString () = s"$asString/${_end}/${_chars.length}"

  //
  // from Line and RLine API

  def length = _end
  def charAt (pos :Int) = _chars(pos)
  // TODO: document, handle, test boundary conditions? or just throw?
  def slice (start :Int, until :Int) = _chars.slice(start, until)
  def sliceString (start :Int, until :Int) = new String(_chars, start, until-start)
  def edited = _edited

  def insert (pos :Int, c :Char) {
    prepInsert(pos, 1)
    _chars(pos) = c
    _end += 1
    _edited.emit(Line.Edit(pos, 0, 1, this))
  }

  def insert (pos :Int, cs :Array[Char]) {
    prepInsert(pos, cs.length)
    System.arraycopy(cs, 0, _chars, pos, cs.length)
    _end += cs.length
    _edited.emit(Line.Edit(pos, 0, cs.length, this))
  }

  def insert (pos :Int, str :String) {
    insert(pos, str.toCharArray)
  }

  def delete (pos :Int, length :Int) {
    val last = pos + length
    assert(pos > 0 && last <= _end)
    System.arraycopy(_chars, last, _chars, pos, _end-last)
    _end -= length
    _edited.emit(Line.Edit(pos, length, 0, this))
  }

  def replace (pos :Int, delete :Int, cs :Array[Char]) {
    val lastDeleted = pos + delete
    assert(lastDeleted <= _end)
    val lastAdded = pos + cs.length

    // if we have a net increase in characters, shift tail right to make room
    val deltaLength = lastAdded - lastDeleted
    if (deltaLength > 0) prepInsert(pos, deltaLength)
    // if we have a net decrease, shift tail left to close gap
    else if (deltaLength < 0) System.arraycopy(
      _chars, lastDeleted, _chars, lastAdded, _end-lastDeleted)
    // otherwise, we've got a perfect match, no shifting needed

    System.arraycopy(cs, 0, _chars, pos, cs.length)
    _end += deltaLength
    _edited.emit(Line.Edit(pos, delete, cs.length, this))
  }

  //
  // impl details

  private def prepInsert (pos :Int, length :Int) {
    assert(pos > 0 && pos <= _end)
    val curlen = _chars.length
    val curend = _end
    // if we need to expand our _chars array...
    if (curend + length > curlen) {
      // ...tack on an extra N characters in expectation of future expansions
      val nchars = new Array[Char](_chars.length+length+LineImpl.ExpandN)
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
