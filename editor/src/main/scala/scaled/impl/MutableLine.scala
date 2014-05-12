//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.Writer
import reactual.{Signal, SignalV}
import scala.annotation.tailrec
import scaled._

/** [MutableLine] related types and utilities. */
object MutableLine {

  /** The number of extra characters padded onto an expanded line. */
  final val ExpandN = 32

  /** An empty character array used for edits that delete no characters. */
  final val NoChars = Array[Char]()

  /** Creates a mutable line with a copy of the contents of `line`. */
  def apply (buffer :BufferImpl, line :LineV) = {
    val cs = new Array[Char](line.length)
    val ss = new Array[Styles](line.length)
    val xs = new Array[Syntax](line.length)
    line.sliceInto(0, line.length, cs, ss, xs, 0)
    new MutableLine(buffer, cs, ss, xs)
  }
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
class MutableLine (
  buffer :BufferImpl, initCs :Array[Char], initSs :Array[Styles], initXs :Array[Syntax]
) extends LineV {
  def this (buffer :BufferImpl, cs :Array[Char]) = this(
    buffer, cs, Array.fill(cs.length)(Styles.None), Array.fill(cs.length)(Syntax.Default))

  require(initCs != null && initSs != null && initXs != null)

  protected var _chars = initCs
  protected var _styles = initSs
  protected var _syns = initXs
  private[this] var _end = initCs.size

  override def length = _end
  override def view (start :Int, until :Int) = new Line(_chars, _styles, _syns, start, until-start)
  override def slice (start :Int, until :Int) =
    new Line(_chars.slice(start, until), _styles.slice(start, until), _syns.slice(start, until))
  override protected def _offset = 0

  /** Writes this mutable line to the supplied writer. */
  def write (out :Writer) = out.write(_chars, 0, _end)

  /** Splits this line at `loc`. Deletes the data from `loc.col` onward from this line.
    * @return a new line which contains the data from `loc.col` onward. */
  def split (loc :Loc) :MutableLine = {
    // TODO: if loc.col is close to zero, just give our internals to the new line and create new
    // internals for ourselves?
    MutableLine(buffer, delete(loc, _end-loc.col))
  }

  /** Inserts `c` into this line at `loc` with styles `styles` and syntax `syntax`. */
  def insert (loc :Loc, c :Char, styles :Styles, syntax :Syntax) {
    prepInsert(loc.col, 1)
    _chars(loc.col) = c
    _styles(loc.col) = styles
    _syns(loc.col) = syntax
    _end += 1
  }

  /** Inserts `[offset, offset+count)` slice of `line` into this line at `loc`. */
  def insert (loc :Loc, line :LineV, offset :Int, count :Int) :Loc = {
    prepInsert(loc.col, count)
    line.sliceInto(offset, offset+count, _chars, _styles, _syns, loc.col)
    _end += count
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
    require(pos >= 0 && last <= _end, s"$pos >= 0 && $last <= ${_end}")
    val deleted = slice(pos, pos+length)
    System.arraycopy(_chars , last, _chars , pos, _end-last)
    System.arraycopy(_styles, last, _styles, pos, _end-last)
    System.arraycopy(_syns  , last, _syns  , pos, _end-last)
    _end -= length
    deleted
  }

  /** Replaces `delete` chars starting at `loc` with `line`. */
  def replace (loc :Loc, delete :Int, line :LineV) :Line = {
    val pos = loc.col
    val lastDeleted = pos + delete
    require(lastDeleted <= _end, s"$lastDeleted <= ${_end} in replace($loc, $delete)")
    val added = line.length
    val lastAdded = pos + added
    val replaced = if (delete > 0) slice(pos, pos+delete) else Line.Empty

    val deltaLength = lastAdded - lastDeleted
    // if we have a net increase in characters, shift tail right to make room
    if (deltaLength > 0) prepInsert(pos, deltaLength)
    // if we have a net decrease, shift tail left to close gap
    else if (deltaLength < 0) {
      val toShift = _end-lastDeleted
      System.arraycopy(_chars , lastDeleted, _chars , lastAdded, toShift)
      System.arraycopy(_styles, lastDeleted, _styles, lastAdded, toShift)
      System.arraycopy(_syns  , lastDeleted, _syns  , lastAdded, toShift)
    }
    // otherwise, we've got a perfect match, no shifting needed

    line.sliceInto(0, added, _chars, _styles, _syns, pos)
    _end += deltaLength
    replaced
  }

  /** Transforms chars with `fn` starting at `loc` and continuiing to column `last`. This results in
    * an edited event that reports the transformed characters as deleted and added, regardless of
    * whether `fn` actually changed them.
    * @return the location after the last transformed char. */
  def transform (fn :Char => Char, loc :Loc, last :Int = length) :Loc = {
    var p = loc.col
    while (p < last) { _chars(p) = fn(_chars(p)) ; p += 1 }
    loc.atCol(last)
  }

  /** Transforms styles (using `fn`) starting at `loc` and continuing to column `last`. If any
    * characters actually change style, a call to `BufferImpl.noteLineStyled` will be made after
    * the style has been applied to the entire region. */
  def updateStyles (fn :Styles => Styles, loc :Loc, last :Int = length) {
    val end = math.min(length, last)
    @tailrec def loop (pos :Int, first :Int) :Int = if (pos >= end) first else {
      val ostyles = _styles(pos) ; val nstyles = fn(ostyles)
      if (nstyles eq ostyles) loop(pos+1, first)
      else {
        _styles(pos) = nstyles
        loop(pos+1, if (first == -1) pos else first)
      }
    }
    val first = loop(loc.col, -1)
    if (first != -1) buffer.noteLineStyled(loc.atCol(first))
  }

  /** Sets the syntax of chars in `[loc,last)` to `syntax`. */
  def setSyntax (syntax :Syntax, loc :Loc, last :Int = length) {
    var p = loc.col ; while (p < last) { _syns(p) = syntax ; p += 1 }
  }

  override def toString () = s"$asString/${_end}/${_chars.length}"

  //
  // impl details

  private def prepInsert (pos :Int, length :Int) {
    require(pos >= 0 && pos <= _end, s"0 <= $pos <= ${_end} ($length)")
    val curlen = _chars.length
    val curend = _end
    val tailpos = pos+length
    val taillen = curend-pos
    // if we need to expand our arrays...
    if (curend + length > curlen) {
      // ...tack on an extra N characters in expectation of future expansions
      val nchars = new Array[Char](_chars.length+length + MutableLine.ExpandN)
      System.arraycopy(_chars, 0, nchars, 0, pos)
      System.arraycopy(_chars, pos, nchars, tailpos, taillen)
      val nstyles = new Array[Styles](nchars.length)
      System.arraycopy(_styles, 0, nstyles, 0, pos)
      System.arraycopy(_styles, pos, nstyles, tailpos, taillen)
      val nsyns = new Array[Syntax](nchars.length)
      System.arraycopy(_syns, 0, nsyns, 0, pos)
      System.arraycopy(_syns, pos, nsyns, tailpos, taillen)
      _chars = nchars
      _styles = nstyles
      _syns = nsyns
    }
    // otherwise shift characters down, if necessary
    else if (pos < curend) {
      System.arraycopy(_chars , pos, _chars , tailpos, taillen)
      System.arraycopy(_styles, pos, _styles, tailpos, taillen)
      System.arraycopy(_syns  , pos, _syns  , tailpos, taillen)
    }
  }
}
