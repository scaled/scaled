//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scaled.util.Chars

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
  * between character offset and line offset plus (intra-line) character offset. This isolates the
  * read-only API, see [[Buffer]] for the mutable API and [[RBuffer]] for the reactive-mutable API.
  *
  * Note: there's only one implementation of buffers, but the API factorization exists to allow
  * differing degrees of control to be exposed depending on the circumstances. If someone should
  * not be allowed to mutate a buffer, they can be supplied with a `BufferV` reference. If they can
  * mutate, but should not be allowed to register (possibly memory leak-inducing) reactive
  * handlers, they can be supplied with a `Buffer` reference. If they should have all the games and
  * all the puzzles, they get an `RBuffer` reference.
  *
  * For better and worse, the JVM allows you to downcast to the more powerful type. That's a bad
  * idea. Don't do it. Consider yourself warned.
  *
  * @define RNLNOTE Note: the last line does not conceptually include a trailing newline, and
  * [[insert(Loc,Seq[LineV])]] takes this into account.
  */
abstract class BufferV extends Region {

  /** The name of this buffer. Tends to be the name of the store from which it was read, but may
    * differ if two buffers exist with the same name or if the buffer is viewing something that did
    * not come from a store. */
  def name :String

  /** The store being edited by this buffer. */
  def store :Store

  /** The current mark, if any. */
  def mark :Option[Loc]

  /** Whether or not this buffer can be edited interactively (by the user). */
  def editable :Boolean

  /** Whether or not this buffer has been modified since it was loaded or last saved. */
  def dirty :Boolean

  /** Whether or not this buffer should be saved before being closed. If it is a transient buffer
    * (was not loaded from a file and has never been written to a file), then it will never report
    * needing to be saved. If it was loaded from a file or written to a file at any point, it
    * will report needing to be saved if it is dirty.
    */
  def needsSave :Boolean = dirty && store.exists

  /** A mapping of buffer-local state. */
  def state :StateV

  /** Returns the position at the start of the buffer. This is always [[Loc.Zero]], but this method
    * exists for symmetry with [[end]]. */
  def start :Loc = Loc.Zero

  /** Returns the position at the end of the buffer. This will be one character past the end of the
    * last line in the buffer. */
  def end :Loc = Loc(lines.size-1, lines.last.length)

  /** Returns a location for the specified character offset into the buffer. If `offset` is greater
    * than the length of the buffer, the returned `Loc` will be positioned after the buffer's final
    * character. */
  def loc (offset :Int) :Loc = {
    assert(offset >= 0)
    val lls = lines
    def seek (off :Int, idx :Int) :Loc = {
      // if we've spilled past the end of our buffer, roll back to one character past the end of
      // the last line in our buffer
      if (idx >= lls.length) Loc(lls.length-1, lls.last.length)
      else {
        val len = lls(idx).length
        // TODO: this assumes a single character line terminator, what about \r\n?
        if (off > len) seek(off-len-1, idx+1)
        else Loc(idx, off)
      }
    }
    seek(offset, 0)
  }

  /** Returns the character offset into the buffer of `loc`. */
  def offset (loc :Loc) :Int = {
    val lineSep = System.lineSeparator // TODO: buffer should know its own line separator?
    @tailrec def offset (row :Int, off :Int) :Int =
      if (row < 0) off else offset(row-1, lines(row).length+lineSep.length+off)
    offset(loc.row-1, 0) + loc.col
  }

  /** A read-only view of the lines in this buffer. */
  def lines :SeqV[LineV]

  /** Returns the `idx`th line. Indices are zero based.
    * @throws IndexOutOfBoundsException if `idx` is not a valid line index. */
  def line (idx :Int) :LineV = lines(idx)

  /** Returns the line referenced by `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def line (loc :Loc) :LineV = line(loc.row)

  /** Returns the length of the line at `idx`, or zero if `idx` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (idx :Int) :Int = if (idx < 0 || idx >= lines.length) 0 else lines(idx).length

  /** Returns the length of the line at `loc`, or zero if `loc` represents a line beyond the end of
    * the buffer or before its start. */
  def lineLength (loc :Loc) :Int = lineLength(loc.row)

  /** Returns the character at `loc`. Returns `0` if `loc.row` is out of the buffer bounds. */
  def charAt (loc :Loc) :Char =
    if (loc.row < 0 || loc.row >= lines.size) 0
    else line(loc.row).charAt(loc.col)

  /** Returns all tags which match `tclass` and overlap `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def tagsAt[T] (tclass :Class[T], loc :Loc) :List[Tag[T]] = line(loc.row).tagsAt(tclass, loc.col)

  /** Returns the first tag matching `tclass` that overlaps `loc`, or None.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def tagAt[T] (tclass :Class[T], loc :Loc) :Option[T] =
    line(loc.row).tagAt(tclass, loc.col, null.asInstanceOf[T]) match {
      case null => None
      case tval => Some(tval)
    }

  /** Returns all tags on the character at `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def tagsAt (loc :Loc) :List[Tag[_]] = line(loc.row).tagsAt(loc.col)

  /** Returns the CSS style classes of the character at `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def stylesAt (loc :Loc) :List[String] = line(loc.row).stylesAt(loc.col)

  /** Returns `stylesAt(loc)` unless `loc` is at the end of its line, in which case the styles
    * for the character preceding `loc` are returned. If the line containing `loc` is empty, empty
    * styles are returned.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def stylesNear (loc :Loc) :List[String] = {
    val line = this.line(loc.row) ; val len = line.length
    line.stylesAt(if (loc.col < len || len == 0) loc.col else len-1)
  }

  /** Returns the syntax tag of the character at `loc`.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def syntaxAt (loc :Loc) :Syntax = line(loc.row).syntaxAt(loc.col)

  /** Returns `syntaxAt(loc)` unless `loc` is at the end of its line, in which case the syntax
    * for the character preceding `loc` is returned. If the line containing `loc` is empty, the
    * default syntax is returned.
    * @throws IndexOutOfBoundsException if `loc.row` is not a valid line index. */
  def syntaxNear (loc :Loc) :Syntax = {
    val line = this.line(loc.row) ; val len = line.length
    line.syntaxAt(if (loc.col < len || len == 0) loc.col else len-1)
  }

  /** Returns a copy of the data between `[start, until)`. $RNLNOTE */
  def region (start :Loc, until :Loc) :Seq[Line] =
    if (until < start) region(until, start)
    else if (start.row == until.row) Seq(line(start).slice(start.col, until.col))
    else {
      val lb = Seq.builder[Line]
      lb  += line(start).slice(start.col)
      lb ++= lines.slice(start.row+1, until.row).map(_.slice(0))
      lb  += lines(until.row).slice(0, until.col)
      lb.build()
    }

  /** Returns a copy of the data in region `r`. $RNLNOTE */
  def region (r :Region) :Seq[Line] = region(r.start, r.end)

  /** Returns the largest region around `loc` which matches the supplied `category`.
    * For example, supply [[Chars.Word]] to obtain the "word" at `loc`. */
  def regionAt (loc :Loc, category :Chars.Category) :Seq[Line] = {
    val lstart = scanBackward(category.isNot, loc)
    val start = if (category.is(charAt(lstart))) lstart else forward(lstart, 1)
    val end = if (!category.is(charAt(start))) start
              else scanForward(category.isNot, loc)
    region(start, end)
  }

  /** Returns the start of the line at `row`. */
  def lineStart (row :Int) :Loc = Loc(row, 0)

  /** Returns the start of the line at `loc.row`. */
  def lineStart (loc :Loc) :Loc = loc.atCol(0)

  /** Returns the end of the line at `row`. */
  def lineEnd (row :Int) :Loc = Loc(row, lineLength(row))

  /** Returns the end of the line at `loc.row`. */
  def lineEnd (loc :Loc) :Loc = loc.atCol(lineLength(loc.row))

  /** Bounds `loc` into this buffer. Its row will be bound to [0, `lines.length`) and its column
    * bound into the line to which its row was bound. */
  def bound (loc :Loc) :Loc = {
    if (loc.row >= lines.size) Loc(lines.size-1, lines.last.length)
    else if (loc.row < 0) Loc(0, lines(0).bound(loc.col))
    else lines(loc.row).bound(loc)
  }

  /** Returns the loc `count` characters forward of `loc`, or [[end]] if we reach it first. */
  def forward (loc :Loc, count :Int) :Loc = {
    @inline @tailrec def seek (row :Int, col :Int, remain :Int) :Loc = {
      val lcol = col + remain
      val llen = lines(row).length
      if (llen >= lcol) Loc(row, lcol)
      else if (row == lines.length-1) end
      else seek(row+1, 0, lcol-llen-1) // -1 to account for line separator
    }
    seek(loc.row, loc.col, count)
  }

  /** Returns the loc `count` characters backward of `loc`, or [[start]] if we reach it first. */
  def backward (loc :Loc, count :Int) :Loc = {
    @inline @tailrec def seek (row :Int, col :Int, remain :Int) :Loc = {
      val lcol = col - remain
      if (lcol >= 0) Loc(row, lcol)
      else if (row == 0) start
      else seek(row-1, line(row-1).length, remain-col-1) // -1 to account for line separator
    }
    seek(loc.row, loc.col, count)
  }

  /** Scans forward from `start` for a character that matches `pred`. If `start` matches, it will be
    * returned. If `stop` is reached before finding a match, `stop` is returned. Note that end of
    * line characters are included in the scan.
    *
    * @param pred a predicate that will be passed the character and syntax at each buffer position.
    */
  def scanForward (pred :(Char,Syntax) => Boolean, start :Loc, stop :Loc = this.end) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = {
      val line = this.line(row)
      val last = if (row == stopr) stopc else line.length
      var p = col ; while (p <= last && !pred(line.charAt(p), line.syntaxAt(p))) p += 1
      if (p <= last) Loc(row, p)
      else if (row == stopr) stop
      else seek(row+1, 0)
    }
    if (start < stop) seek(start.row, start.col) else stop
  }

  /** Scans backward from the location immediately previous to `start` for a character that matches
    * `pred`. If `stop` is reached before finding a match, `stop` is returned. Note that end of
    * line characters are included in the scan.
    *
    * @param pred a predicate that will be passed the character and syntax at each buffer position.
    */
  def scanBackward (pred :(Char,Syntax) => Boolean, start :Loc, stop :Loc = this.start) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = {
      val line = this.line(row)
      val first = if (row == stopr) stopc else 0
      var p = col ; while (p >= first && !pred(line.charAt(p), line.syntaxAt(p))) p -= 1
      if (p >= first) Loc(row, p)
      else if (row == stopr) stop
      else seek(row-1, this.line(row-1).length)
    }
    if (stop < start) seek(start.row, start.col-1) else stop
  }

  /** Scans forward from `start` while `pred` continues to match. Returns the last location at
    * which `pred` matches. Returns `start` if we fail immediately. If `stop` is reached before
    * finding a non-matching character, `stop` is returned. Note that end of line characters are
    * included in the scan.
    *
    * @param pred a predicate that will be passed the character and syntax at each buffer position.
    */
  def scanForWhile (pred :(Char,Syntax) => Boolean, start :Loc, stop :Loc = this.end) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = {
      val line = this.line(row)
      val last = if (row == stopr) stopc else line.length
      var p = col ; while (p <= last && pred(line.charAt(p), line.syntaxAt(p))) p += 1
      if (p <= last) backward(Loc(row, p), 1)
      else if (row == stopr) stop
      else seek(row+1, 0)
    }
    if (start < stop) seek(start.row, start.col) else stop
  }

  /** Scans backward from the location immediately previous to `start` while `pred` continues to
    * match. Returns the last location at which `pred` matches. Returns `start` if we failed
    * immediately. If `stop` is reached before finding a non-matching character, `stop` is returned.
    * Note that end of line characters are included in the scan.
    *
    * @param pred a predicate that will be passed the character and syntax at each buffer position.
    */
  def scanBackWhile (pred :(Char,Syntax) => Boolean, start :Loc, stop :Loc = this.start) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = {
      val line = this.line(row)
      val first = if (row == stopr) stopc else 0
      var p = col ; while (p >= first && pred(line.charAt(p), line.syntaxAt(p))) p -= 1
      if (p >= first) forward(Loc(row, p), 1)
      else if (row == stopr) stop
      else seek(row-1, this.line(row-1).length)
    }
    if (stop < start) seek(start.row, start.col-1) else stop
  }

  /** Scans forward from the `start`th lne of the buffer, seeking a line that matches `pred`.
    *
    * @return the index of the first matching row. If the `stop`th line is reached without a match,
    * `stop` is returned.
    */
  def scanRowForward (pred :LineV => Boolean, start :Int, stop :Int = lines.size-1) :Int = {
    @inline @tailrec def loop (row :Int) :Int =
      if (row == stop) stop else if (pred(line(row))) row else loop(row+1)
    loop(start)
  }

  /** Scans backward from the `start`th lne of the buffer, seeking a line that matches `pred`.
    *
    * @return the index of the first matching row. If the `stop`th line is reached without a match,
    * `stop` is returned.
    */
  def scanRowBackward (pred :LineV => Boolean, start :Int, stop :Int = 0) :Int = {
    @inline @tailrec def loop (row :Int) :Int =
      if (row == stop) stop else if (pred(line(row))) row else loop(row-1)
    loop(start)
  }

  /** Searches forward from `start` for the first match of `m`, stopping before `stop` (`stop` is
    * not checked for a match).
    *
    * @return the location of the match or `Loc.None`.
    */
  def findForward (m :Matcher, start :Loc, stop :Loc = this.end) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = line(row).indexOf(m, col) match {
      case -1 => if (row == stopr) Loc.None else seek(row+1, 0)
      case ii => if (row == stopr && ii + m.matchLength > stopc) Loc.None else Loc(row, ii)
    }
    if (start < stop) seek(start.row, start.col) else Loc.None
  }

  /** Searches backward from the location immediately previous to `start` for the first match of
    * `m`, stopping when `stop` is reached (`stop` is checked for a match).
    *
    * @return the location of the match or `Loc.None`.
    */
  def findBackward (m :Matcher, start :Loc, stop :Loc = this.start) :Loc = {
    val stopr = stop.row ; val stopc = stop.col
    @inline @tailrec def seek (row :Int, col :Int) :Loc = line(row).lastIndexOf(m, col) match {
      case -1 => if (row == stopr) Loc.None else seek(row-1, line(row-1).length)
      case ii => if (row == stopr && ii < stopc) Loc.None else Loc(row, ii)
    }
    if (start > stop) seek(start.row, start.col-1) else Loc.None
  }
}

/** Extends [[BufferV]] with a mutation API. See [[RBuffer]] for a further extension which provides
  * the ability to register to react to changes.
  */
abstract class Buffer extends BufferV {

  /** Requests that this buffer be killed. Any pre-kill hooks will be executed, and assuming none
    * of the hooks abort the kill process, the buffer will be unloaded from its corresponding
    * workspace and any live views of the buffer will be closed. */
  def kill () :Unit

  /** Saves this buffer to its current store. If the buffer is not dirty, NOOPs. */
  def save () {
    // the caller should check dirty and provide feedback, but let's nip funny biz in the bud
    if (dirty) saveTo(store)
  }

  /** Saves this buffer to `store`, updating [[store]] and [[name]] appropriately. */
  def saveTo (store :Store) :Unit

  /** Configures whether this buffer can be edited interactively (by the user). */
  def editable_= (editable :Boolean) :Unit

  /** Marks this buffer as clean. In general one should not use this method, as a buffer will
    * automatically be marked clean on being saved. However, some buffers are special and don't
    * represent the contents of a file and may wish to manage cleanliness specially. */
  def markClean () :Unit

  /** Sets the current mark to `loc`. The mark will be [[bound]] into the buffer. */
  def mark_= (loc :Loc) :Unit
  /** Clears the current mark. */
  def clearMark () :Unit

  /** That which handles undoing and redoing for this buffer. */
  def undoer :Undoer

  /** Inserts the single character `c` into this buffer at `loc` tagged with syntax `syntax`.
    * @return the buffer location just after the inserted character. */
  def insert (loc :Loc, c :Char, syntax :Syntax) :Loc

  /** Inserts the contents of `line` into this buffer at `loc`. The line in question will be
    * spliced into the line at `loc`, a new line will not be created. Use [[insertLine]] if you
    * wish to break the line immediately following the inserted line.
    *
    * @return the buffer location just after the inserted line. */
  def insert (loc :Loc, line :LineV) :Loc

  /** Inserts the contents of `line` into this buffer at `loc` and splits the line immediately
    * following the inserted line. This effectively inserts a line plus a newline.
    *
    * @return the buffer location just after the inserted newline. */
  def insertLine (loc :Loc, line :LineV) :Loc = split(insert(loc, line))

  /** Inserts `region` into this buffer at `loc`. `region` will often have come from a call to
    * [[region(Loc,Loc)]] or [[delete(Loc,Loc)]].
    *
    * The lines will be spliced into the line at `loc` which is almost certainly what you want.
    * This means the line at `loc` will be [[split]] in two, the first line in `region` will be
    * appended to the first half of the split line, and the last line `region` will be prepended to
    * the last half of the split line; the lines in between (if any) are inserted as is. If
    * `region` is length 1 this has the same effect as [[insert(Loc,Line)]].
    *
    * @return the buffer location just after the end of the inserted region.
    */
  def insert (loc :Loc, region :Ordered[LineV]) :Loc

  /** Appends `region` to the end of this buffer.
    * @return the buffer location just after the end of the appended region.
    */
  def append (region :Ordered[LineV]) :Loc = insert(end, region)

  /** Deletes `count` characters from the line at `loc`.
    * @return the deleted chars as a line. */
  def delete (loc :Loc, count :Int) :Line
  /** Deletes the data between `[start, until)` from the buffer. Returns a copy of the deleted data.
    * $RNLNOTE */
  def delete (start :Loc, until :Loc) :Seq[Line]
  /** Deletes the data in region `r` from the buffer. Returns a copy of the deleted data. $RNLNOTE */
  def delete (r :Region) :Seq[Line] = delete(r.start, r.end)

  /** Replaces `count` characters in the line at `loc` with the `line`.
    * @return the replaced characters as a line. */
  def replace (loc :Loc, count :Int, line :LineV) :Line
  /** Replaces the region between `[start, until)` with `lines`.
    * @return the buffer location just after the replaced region. */
  def replace (start :Loc, until :Loc, lines :Ordered[LineV]) :Loc
  /** Replaces the region `r` with `lines`.
    * @return the buffer location just after the replaced region. */
  def replace (r :Region, lines :Ordered[LineV]) :Loc = replace(r.start, r.end, lines)

  /** Transforms the characters between `[start, until)` using `fn`.
    * @return the buffer location just after the transformed region. */
  def transform (start :Loc, until :Loc, fn :Char => Char) :Loc
  /** Transforms the characters in region `r` using `fn`.
    * @return the buffer location just after the transformed region. */
  def transform (r :Region, fn :Char => Char) :Loc = transform(r.start, r.end, fn)

  /** Splits the line at `loc`. The characters up to `loc.col` will remain on the `loc.row`th line,
    * and the character at `loc.col` and all subsequent characters will be moved to a new line
    * which immediately follows the `loc.row`th line.
    * @return the new location of the character that was at `loc`. */
  def split (loc :Loc) :Loc

  /** Sets the syntax of the characters between `[start, until)` to `syntax`. */
  def setSyntax (syntax :Syntax, start :Loc, until :Loc) :Unit
  /** Sets the syntax of the characters in region `r` to `syntax`. */
  def setSyntax (syntax :Syntax, r :Region) :Unit = setSyntax(syntax, r.start, r.end)

  /** Tags the region `[start, until)` with `tag`. */
  def addTag[T] (tag :T, start :Loc, until :Loc) :Unit
  /** Tags the region `r` with `tag`. */
  def addTag[T] (tag :T, r :Region) :Unit = addTag(tag, r.start, r.end)
  /** Removes the specified tag from the region `[start, until)`. If an existing tag partially
    * overlaps the region, it will be split at the region boundaries and the overlapping fragment
    * removed. */
  def removeTag[T] (tag :T, start :Loc, until :Loc) :Unit
  /** Removes the specified tag from the region `r`. If an existing tag partially overlaps the
    * region, it will be split at the region boundaries and the overlapping fragment removed. */
  def removeTag[T] (tag :T, r :Region) :Unit = removeTag(tag, r.start, r.end)

  /** Removes tags which overlap `[start, until)`, match `tclass` and for which `pred` is true.
    * Unlike [[removeTag]] this removes entire tags, it does not split overlappers. */
  def removeTags[T] (tclass :Class[T], pred :T => Boolean, start :Loc, until :Loc) :Unit
  /** Removes tags which overlap `r`, match `tclass` and for which `pred` is true.
    * Unlike [[removeTag]] this removes entire tags, it does not split overlappers. */
  def removeTags[T] (tclass :Class[T], pred :T => Boolean, r :Region) :Unit =
    removeTags(tclass, pred, r.start, r.end)

  /** Adds `tag` to the `idx`th line. Any existing tag with the same key will be replaced. */
  def setLineTag[T <: Line.Tag] (idx :Int, tag :T) :Unit
  /** Adds `tag` to the `loc`th line. Any existing tag with the same key will be replaced. */
  def setLineTag[T <: Line.Tag] (loc :Loc, tag :T) :Unit = setLineTag(loc.row, tag)
  /** Clears any line tag matching `key` from the `idx`th line. */
  def clearLineTag (idx :Int, key :Any) :Unit
  /** Clears any line tag matching `key` from the `idx`th line. */
  def clearLineTag (loc :Loc, key :Any) :Unit = clearLineTag(loc.row, key)

  /** Adds CSS style class `style` to the characters between `[start, until)`. */
  def addStyle (style :String, start :Loc, until :Loc) = addTag(style.intern, start, until)
  /** Adds CSS style class `style` to the characters in region `r`. */
  def addStyle (style :String, r :Region) :Unit = addStyle(style, r.start, r.end)
  /** Removes CSS style class `style` from the characters between `[start, until)`. */
  def removeStyle (style :String, start :Loc, until :Loc) = removeTag(style, start, until)
  /** Removes CSS style class `style` from the characters in region `r`. */
  def removeStyle (style :String, r :Region) :Unit = removeStyle(style, r.start, r.end)
}

/** `Buffer` related types and utilities. */
object Buffer {

  /** Conveys information about a buffer edit. See [[Insert]], [[Delete]], [[Transform]]. */
  sealed trait Edit extends Region with Undoable {
    /** The start of this edit. */
    def start :Loc
    /** The end of this edit. */
    def end :Loc
  }

  /** An event emitted when text is inserted into a buffer. */
  class Insert (val start :Loc, val end :Loc, buffer :Buffer) extends Edit {
    def undo () = buffer.delete(start, end)
    def merge (other :Insert) :Insert = {
      assert(end == other.start, "Cannot merge non-adjacent $this and $other")
      new Insert(start, other.end, buffer)
    }
    override def toString = s"Edit[+${Region.toString(this)}]"
  }
  object Insert {
    def unapply (edit :Insert) = Some((edit.start, edit.end))
  }

  /** An event emitted when text is deleted from a buffer. */
  class Delete (val start :Loc, val deletedRegion :Seq[Line], buffer :Buffer) extends Edit {
    val end :Loc = start + deletedRegion
    def undo () = buffer.insert(start, deletedRegion)
    override def toString = s"Edit[-${Region.toString(this)}]"
  }
  object Delete {
    def unapply (edit :Delete) = Some((edit.start, edit.end, edit.deletedRegion))
  }

  /** An event emitted when text in a buffer is transformed. */
  class Transform (val start :Loc, val original :Seq[Line], buffer :Buffer) extends Edit {
    val end :Loc = start + original
    def undo () = buffer.replace(start, end, original)
    override def toString = s"Edit[!${Region.toString(this)}]"
  }
  object Transform {
    def unapply (edit :Transform) = Some((edit.start, edit.end, edit.original))
  }

  /** Returns true if `name` is a temporary buffer name. */
  def isScratch (name :String) = (name startsWith "*") && (name endsWith "*")

  /** Creates a read-only buffer view. Mainly useful for testing. */
  def apply (name :String, text :String) :BufferV = apply(name, Line.fromText(text))

  /** Creates a read-only buffer view. Mainly useful for testing. */
  def apply (_name :String, _lines :Seq[LineV]) :BufferV = new BufferV() {
    def name = _name
    val store = new TextStore(name, "", "")
    def mark = None
    def editable = false
    def dirty = false
    val state = new State()
    def lines = _lines
  }
}

/** The reactive version of [Buffer]. */
abstract class RBuffer extends Buffer {

  /** A reactive view of [[name]]. */
  def nameV :ValueV[String]

  /** A reactive view of [[store]]. This value is forcibly updated when the buffer is saved (even if
    * the file did not change). One can thus listen to this value to react to all buffer saves. */
  def storeV :ValueV[Store]

  /** The current mark, if any. */
  def markV :ValueV[Option[Loc]]

  /** A reactive view of [[editable]]. */
  def editableV :ValueV[Boolean]

  /** A reactive view of [[dirty]]. */
  def dirtyV :ValueV[Boolean]

  /** A signal that is emitted when this buffer is about to be saved. */
  def willSave :SignalV[Buffer]

  /** A signal emitted when this buffer has been killed. */
  def killed :SignalV[Buffer]

  /** A signal emitted when this buffer is edited. */
  def edited :SignalV[Buffer.Edit]

  /** A signal emitted when a line in this buffer has a CSS style applied to it. The emitted `Loc`
    * will contain the row of the line that was edited and the positon of the earliest character to
    * which a style was applied. Zero or more additional style changes may have been made to
    * characters after the one identified by the `Loc`, but none will have been made to characters
    * before that `Loc`. */
  def lineStyled :SignalV[Loc]

  /** A reactive mapping of buffer-local state. */
  override val state :State = new State()

  // implement some Buffer methods in terms of our reactive values
  override def name = nameV()
  override def store = storeV()
  override def mark = markV()
  override def editable = editableV()
  override def dirty = dirtyV()
}
