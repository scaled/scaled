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

  /** Returns the character at `pos`. If `pos` is outside `[0,length)` 0 is returned. */
  def charAt (pos :Int) :Char = if (pos >= 0 && pos < length) _chars(_offset+pos) else 0

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

  /** Returns the index of the first occurance of `ch` at pos `from` or later.
    * Returns -1 if `ch` is not found. */
  def indexOf (ch :Char, from :Int = 0) :Int = {
    val end = length
    var pos = from ; while (pos < end && _chars(pos) != ch) pos += 1
    if (pos == end) -1 else pos
  }

  /** Returns the index of the first occurance of `ch` at pos `from` or earlier.
    * Returns -1 if `ch` is not found. */
  def lastIndexOf (ch :Char, from :Int = length-1) :Int = {
    var pos = from ; while (pos >= 0 && _chars(pos) != ch) pos -= 1
    pos
  }

  /** Returns the index of the first character that matches `pred` at pos `from` or later.
    * Returns -1 if no character matched. */
  def find (pred :Char => Boolean, from :Int = 0) :Int = {
    val end = length
    var pos = from ; while (pos < end && !pred(_chars(pos))) pos += 1
    if (pos == end) -1 else pos
  }

  /** Returns the offset into this line at which the characters of `cs` in `[offset, length)` are
    * matched, starting from `start`. -1 is returned if no match could be found. */
  final def search (cmp :(Char, Char) => Boolean, cs :Array[Char], offset :Int, length :Int,
                    start :Int) :Int = {
    if (start + length > this.length) -1
    else {
      val tcs = _chars ; val last = this.length - length
      @tailrec @inline def check (toffset :Int, ii :Int) :Int =
        if (ii < length && cmp(cs(offset+ii), tcs(toffset+ii))) check(toffset, ii+1) else ii
      var ss = start ; while (ss <= last && check(_offset+ss, 0) != length) ss += 1
      if (ss > last) -1 else ss
    }
  }

  /** Returns true if the characters of `cs` in `[offset, length)` are equal to the characters in
    * this line in `[start, length)`. */
  final def matches (cmp :(Char, Char) => Boolean, cs :Array[Char], offset :Int, length :Int,
                     start :Int) :Boolean = {
    if (start + length > this.length) false
    else {
      val tcs = _chars ; val toffset = _offset + start
      var ii = 0 ; while (ii < length && cmp(cs(offset+ii), tcs(toffset+ii))) ii += 1
      ii == length
    }
  }

  /** Returns true if the styles of `ss` in `[offset, length)` are equal to the styles in this line
    * in `[start, length)`. */
  final def styleMatches (ss :Array[Styles], offset :Int, length :Int, start :Int) :Boolean = {
    if (start + length > this.length) false
    else {
      val tss = _styles ; val toffset = _offset + start
      var ii = 0 ; while (ii < length && (ss(offset+ii) eq tss(toffset+ii))) ii += 1
      ii == length
    }
  }

  /** Returns the offset into this line at which the characters of `line` are matched, starting from
    * `start`. -1 is returned if no match could be found. */
  def search (cmp :(Char, Char) => Boolean, line :LineV, start :Int = 0) :Int =
    search(cmp, line._chars, line._offset, line.length, start)

  /** Returns the offset into this line at which `[offset, length)` characters of `line` are matched,
    * starting from `start`. -1 is returned if no match could be found. */
  def search (cmp :(Char, Char) => Boolean, line :LineV, offset :Int, length :Int, start :Int) :Int =
    search(cmp, line._chars, line._offset+offset, length, start)

  /** Returns true if the characters of `line` are equal to the characters in this line starting at
    * `start`. */
  def matches (cmp :(Char, Char) => Boolean, line :LineV, start :Int = 0) :Boolean =
    matches(cmp, line._chars, line._offset, line.length, start)

  /** Returns true if the `[offset, length)` characters of `line` are equal to the `[start, length)`
    * characters in this line. */
  def matches (cmp :(Char, Char) => Boolean, line :LineV, offset :Int, length :Int,
               start :Int) :Boolean =
    matches(cmp, line._chars, line._offset+offset, length, start)

  /** Returns the contents of this line as a string. */
  def asString :String = new String(_chars, _offset, length)

  override def subSequence (start :Int, end :Int) = new String(_chars, _offset+start, end-start)

  override def equals (other :Any) = other match {
    case ol :LineV => length == ol.length && ol.matches(Line.exact, this) &&
      ol.styleMatches(_styles, _offset, length, 0)
    case _         => false
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

  /** A case-exact character comparison function. */
  final val exact = (have :Char, want :Char) => have == want

  /** A case-insensitive character comparison function. NOTE: this expects the sought characters to
    * always be lower case. */
  final val loose = (have :Char, want :Char) => {
    have == want || have == Character.toLowerCase(want)
  }

  /** Returns a comparison function for use in searching for `sought`. If `sought` contains all
    * lower-case characters, a case insensitive function is returned, otherwise an exact case
    * function is returned.
    */
  def compFor (sought :Seq[LineV]) :(Char,Char) => Boolean = {
    @inline @tailrec def mixedCase (line :LineV, ii :Int) :Boolean =
      if (ii == line.length) false
      else if (Character.isUpperCase(line.charAt(ii))) false
      else mixedCase(line, ii+1)
    if (sought.exists(mixedCase(_, 0))) exact else loose
  }

  /** Creates one or more lines from the supplied text. If the text contains newlines, it will be
    * split into multiple `Line` instances based thereon. Carriage returns are ignored. Internally,
    * Scaled requires multiline strings to be separated only by newlines. Only text read from the
    * filesystem is allowed to use CR or CRLF. */
  def fromText (text :String) :Seq[Line] = text.split("\n").map(new Line(_))
}
