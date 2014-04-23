//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.util.Arrays

import scala.annotation.tailrec

import reactual.SignalV

/** Models a single line of text, which may or may not be part of a buffer.
  *
  * Lines may be created externally from character or string data or they may be obtained as part
  * of a buffer. In the former case they are immutable and will have type [[Line]], in the latter
  * case they are a view on mutable data and have type [[LineV]]. Do not retain a reference to a
  * [[LineV]] as it may change after you relinquish control of the editor thread.
  */
abstract class LineV extends CharSequence {

  /** The length (in characters) of this line. */
  def length :Int

  /** Returns the character at `pos`. If `pos == length` `\n` is returned. Otherwise if `pos` is
    * outside `[0,length]` 0 is returned. */
  def charAt (pos :Int) :Char = {
    val l = length
    if (pos >= 0 && pos < l) _chars(_offset+pos) else if (pos == l) '\n' else 0
  }

  /** Returns the CSS style classes applied to the character at `pos`, if any. */
  def stylesAt (pos :Int) :Styles = if (pos < length) _styles(_offset+pos) else Styles.None

  /** Bounds the supplied column into this line. This adjusts it to be in [0, [[length]]] (inclusive
    * of the length because the point can be after the last char on this line). */
  def bound (col :Int) :Int = math.max(0, math.min(length, col))

  /** Bounds the supplied loc into this line by bounding its column via [[bound(Int)]]. */
  def bound (loc :Loc) :Loc = loc.atCol(bound(loc.col))

  /** Returns a view of the specified slice of this line. */
  def view (start :Int, until :Int = length) :LineV

  /** Extracts the specified region of this line into a new line.
    * @param start the index of the first character to include in the slice.
    * @param until one past the index of the last character to include in the slice. */
  def slice (start :Int, until :Int = length) :Line

  /** Copies `[start, until)` from this line into `cs`/`ss` at `offset`. */
  def sliceInto (start :Int, until :Int, cs :Array[Char], ss :Array[Styles], offset :Int) {
    System.arraycopy(_chars, _offset+start, cs, offset, until-start)
    System.arraycopy(_styles, _offset+start, ss, offset, until-start)
  }

  /** Returns the characters in `[start, until)` as a string. */
  def sliceString (start :Int, until :Int) :String = new String(_chars, _offset+start, until-start)

  /** Returns a new line which contains `other` appended to `this`. */
  def merge (other :LineV) :Line = {
    val cs = new Array[Char](length + other.length)
    val ss = new Array[Styles](cs.length)
    sliceInto(0, length, cs, ss, 0)
    other.sliceInto(0, other.length, cs, ss, length)
    new Line(cs, ss)
  }

  /** Returns the index of the first occurrence of `ch` at pos `from` or later.
    * Returns -1 if `ch` is not found. */
  def indexOf (ch :Char, from :Int) :Int = {
    val offset = _offset ; val end = length
    var pos = from ; while (pos < end && _chars(offset+pos) != ch) pos += 1
    if (pos == end) -1 else pos
  }
  /** Returns the index of the first occurrence of `ch`. Returns -1 if `ch` is not found. */
  def indexOf (ch :Char) :Int = indexOf(ch, 0)

  /** Returns the index of the first occurrence of `ch` at pos `from` or earlier. Returns -1 if `ch`
    * is not found. */
  def lastIndexOf (ch :Char, from :Int) :Int = {
    val offset = _offset
    var pos = from ; while (pos >= 0 && _chars(offset+pos) != ch) pos -= 1
    pos
  }
  /** Returns the index of the first occurrence of `ch` seeking backward from the end of the line.
    * Returns -1 if `ch` is not found. */
  def lastIndexOf (ch :Char) :Int = lastIndexOf(ch, length-1)

  /** Returns the index of the first character that matches `pred` at pos `from` or later.
    * Returns -1 if no character matched. */
  def indexOf (pred :Char => Boolean, from :Int) :Int = {
    val offset = _offset ; val end = length
    var pos = from ; while (pos < end && !pred(_chars(offset+pos))) pos += 1
    if (pos == end) -1 else pos
  }
  /** Returns the index of the first character that matches `pred`. Returns -1 for no match. */
  def indexOf (pred :Char => Boolean) :Int = indexOf(pred, 0)

  /** Returns the index of the first character that matches `pred` at pos `from` or earlier.
    * Returns -1 if no character matched. */
  def lastIndexOf (pred :Char => Boolean, from :Int) :Int = {
    val offset = _offset
    var pos = from ; while (pos >= 0 && !pred(_chars(offset+pos))) pos -= 1
    pos
  }
  /** Returns the index of the first character that matches `pred`, starting at the last character of
    * the line and seeking backwards. Returns -1 for no match. */
  def lastIndexOf (pred :Char => Boolean) :Int = lastIndexOf(pred, length-1)

  /** Returns the first offset into this line at which `m` matches, starting from `from`.
    * -1 is returned if no match is found. */
  def indexOf (m :Matcher, from :Int = 0) :Int = {
    val offset = _offset ; val n = m.search(_chars, offset+from, offset+length)
    if (n == -1) n else n - offset
  }

  /** Returns the last offset into this line at which `m` matches, starting from `from`.
    * -1 is returned if no match could be found. */
  def lastIndexOf (m :Matcher, from :Int = length-1) :Int = {
    val offset = _offset ; val n = m.searchBackward(_chars, offset, offset+length, offset+from)
    if (n == -1) n else n - offset
  }

  /** Returns true if `m` matches this line starting at `start`. */
  def matches (m :Matcher, start :Int = 0) :Boolean =
    m.matches(_chars, _offset+start, _offset+length)

  /** Returns true if the styles of `ss` in `[offset, length)` are equal to the styles in this line
    * in `[start, length)`. */
  def styleMatches (ss :Array[Styles], offset :Int, length :Int, start :Int) :Boolean = {
    if (start + length > this.length) false
    else {
      val tss = _styles ; val toffset = _offset + start
      var ii = 0 ; while (ii < length && (ss(offset+ii) eq tss(toffset+ii))) ii += 1
      ii == length
    }
  }

  /** Returns the contents of this line as a string. */
  def asString :String = new String(_chars, _offset, length)

  override def subSequence (start :Int, end :Int) = new String(_chars, _offset+start, end-start)

  override def equals (other :Any) = other match {
    case ol :LineV =>
      @inline @tailrec def charsEq (ii :Int) :Boolean = {
        if (ii == length) true else _chars(ii) == ol._chars(ii) && charsEq(ii+1)
      }
      length == ol.length && charsEq(0) && ol.styleMatches(_styles, _offset, length, 0)
    case _ => false
  }

  override def hashCode = {
    @tailrec def loop (code :Int, chars :Array[Char], ii :Int, last :Int) :Int =
      if (ii == last) code else loop(31*code + chars(ii), chars, ii+1, last)
    loop(1, _chars, _offset, _offset+length)
  }

  /** Returns the `char` array that backs this line. The returned array will only be used to
    * implement read-only methods and will never be mutated. */
  protected def _chars :Array[Char]

  /** Returns the `Styles` array that backs this line. The returned array will only be used to
    * implement read-only methods and will never be mutated. */
  protected def _styles :Array[Styles]

  /** Returns the offset into [[_chars]] and [[_styles]] at which our data starts. */
  protected def _offset :Int
}

/** Models a single immutable line of text that is not associated with a buffer.
  *
  * The constructor takes ownership of the supplied arrays. Do not mutate them after using them to
  * create a `Line`. Clone them first if you need to retain the ability to mutate the arrays.
  */
class Line (_cs :Array[Char], _ss :Array[Styles], protected val _offset :Int,
            val length :Int) extends LineV {
  def this (cs :Array[Char], ss :Array[Styles]) = this(cs, ss, 0, cs.length)
  def this (cs :Array[Char], styles :Styles) = this(cs, Array.fill(cs.length)(styles))
  def this (s :String, styles :Styles) = this(s.toCharArray, styles)
  def this (s :String) = this(s.toCharArray, Styles.None)

  require(_cs != null && _ss != null && _offset >= 0 && length >= 0,
          s"Invalid Line args ${_cs} ${_ss} ${_offset} $length")

  override def view (start :Int, until :Int) =
    if (start == 0 && until == length) this else slice(start, until)
  override def slice (start :Int, until :Int) = new Line(_cs, _ss, _offset+start, until-start)

  override protected def _chars = _cs
  override protected def _styles = _ss

  override def toString () = s"$asString [${_offset}:$length/${_cs.length}]"
}

/** `Line` related types and utilities. */
object Line {

  /** An empty line. */
  final val Empty = new Line(Array[Char](), Array[Styles]())

  /** Creates one or more lines from the supplied text. If the text contains newlines, it will be
    * split into multiple `Line` instances based thereon. Carriage returns are ignored. Internally,
    * Scaled requires multiline strings to be separated only by newlines. Only text read from the
    * filesystem is allowed to use CR or CRLF. */
  def fromText (text :String) :Seq[Line] = text.split("\n").map(new Line(_))
}
