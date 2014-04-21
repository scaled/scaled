//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import java.io.{Reader, BufferedReader, BufferedWriter, File, FileReader, FileWriter, StringReader}
import reactual.{Signal, SignalV, Value, ValueV}

import scaled._

// TODO
//
// - Buffer.Loc which demarks a point in a buffer; allow converting to/from buffer char_offset, and
// to/from line_no+char_offset
//
// - Buffer.Anchor which demarks a point in a buffer which is automatically adjusted as the buffer
// is edited (and likely reports to listeners when it's location changes)

/** [BufferImpl] related types and utilities. */
object BufferImpl {

  /** Reads the contents of `reader` info a buffer. Note that the caller is responsible for closing
    * the reader if necessary. */
  def apply (name :String, file :File, reader :Reader) :BufferImpl = {
    val buffed = new BufferedReader(reader)
    val lines = ArrayBuffer[Array[Char]]()
    var line :String = buffed.readLine()
    while (line != null) {
      lines += line.toCharArray
      line = buffed.readLine()
    }
    // TEMP: tack a blank line on the end to simulate a trailing line sep
    lines += MutableLine.NoChars
    new BufferImpl(name, file, lines)
  }

  /** Reads the contents of `file` into a buffer. */
  def fromFile (file :File) :BufferImpl = {
    val cfile = file.getCanonicalFile
    // TODO: use a CharSetDecoder and ByteBuffer + CharBuffer to read things ourselves, and track
    // where and what the line separators are, and whether there's a trailing line sep
    val reader = new FileReader(file)
    try {
      apply(cfile.getName, cfile, reader)
    } finally {
      reader.close
    }
  }

  /** Returns a blank buffer to be used by scratch views (e.g. the minibuffer). */
  def scratch (name :String) :BufferImpl = apply(name, cwd(), new StringReader(""))

  /** Returns an empty buffer with the specified `name` and `file`. The file is expected not to
    * exist. */
  def empty (name :String, file :File) :BufferImpl = apply(name, file, new StringReader(""))

  /** An empty line sequence used for edits that delete no lines. */
  private final val NoLines = Seq[Line]()
}

/** Implements [Buffer] and [RBuffer]. This is where all the excitement happens. */
class BufferImpl private (
  initName :String, initFile :File, initLines :ArrayBuffer[Array[Char]]
) extends RBuffer {
  import Buffer._

  // TODO: character encoding
  // TODO: line endings

  private val _lines = initLines.map(new MutableLine(this, _))
  private val _name = Value(initName)
  private val _file = Value(initFile)
  private val _mark = Value(None :Option[Loc])
  private val _dirty = Value(false)
  private val _edited = Signal[Edit]()
  private val _lineStyled = Signal[Loc]()
  private var _maxRow = longest(_lines, 0, _lines.length)

  val undoStack = new UndoStack(this)

  //
  // from Buffer and RBuffer API

  override def nameV = _name
  override def fileV = _file
  override def markV = _mark
  override def mark_= (loc :Loc) = _mark() = Some(bound(loc))
  override def clearMark () = _mark() = None
  override def undoer = undoStack
  override def edited = _edited
  override def lineStyled = _lineStyled
  override def lines = _lines
  override def maxLineLength = lines(_maxRow).length
  // refine return type to ease life for internal friends
  override def dirtyV :Value[Boolean] = _dirty
  override def line (idx :Int) :MutableLine = _lines(idx)
  override def line (loc :Loc) :MutableLine = _lines(loc.row)

  override def saveTo (file :File) {
    // TODO: file encoding?
    val temp = new File(file.getParentFile, file.getName() + "~")
    val out = new BufferedWriter(new FileWriter(temp))
    try {
      def write (idx :Int) {
        if (idx < lines.length) {
          val l = lines(idx)
          if (idx > 0) out.newLine() // TODO: use newlines we detected when reading the file
          l.write(out)
          write(idx+1)
        }
      }
      write(0)
      out.close()
      temp.renameTo(file)
    } finally {
      temp.delete()
    }

    _file() = file
    _name() = file.getName
    _dirty() = false
  }

  override def loc (offset :Int) = {
    assert(offset >= 0)
    def seek (off :Int, idx :Int) :Loc = {
      // if we've spilled past the end of our buffer, roll back to one character past the end of
      // the last line in our buffer
      if (idx >= _lines.length) Loc(_lines.length-1, _lines.last.length)
      else {
        val len = _lines(idx).length
        // TODO: this assumes a single character line terminator, what about \r\n?
        if (off > len) seek(off-len-1, idx+1)
        else Loc(idx, off)
      }
    }
    seek(offset, 0)
  }

  override def offset (loc :Loc) = {
    @tailrec def offset (row :Int, off :Int) :Int =
      if (row < 0) off else offset(row-1, lines(row).length+lineSep.length+off)
    offset(loc.row-1, 0) + loc.col
  }

  override def region (start :Loc, until :Loc) =
    if (until < start) region(until, start)
    else if (start.row == until.row) Seq(line(start).slice(start.col, until.col))
    else {
      val middle = lines.slice(start.row+1, until.row).map(_.slice(0))
      val s = line(start)
      s.slice(start.col) +=: middle
      middle += _lines(until.row).slice(0, until.col)
      middle.toSeq
    }

  override def insert (loc :Loc, c :Char, styles :Styles) {
    _lines(loc.row).insert(loc, c, styles)
    noteInsert(loc, loc.nextC)
  }
  override def insert (loc :Loc, line :LineV) = {
    val end = _lines(loc.row).insert(loc, line)
    noteInsert(loc, end)
  }
  override def insert (loc :Loc, region :Seq[LineV]) = region.size match {
    case 0 => loc
    case 1 => insert(loc, region.head)
    case _ =>
      // split the first line in twain
      val tail = line(loc).split(loc)
      // merge the first half of the split line with the first line of the region
      line(loc).append(loc, region.head)
      // merge the last line of the region with the second half of the split line
      tail.insert(Loc.Zero, region.last, 0, region.last.length)
      // finally add the middle lines (unmodified) and the merged last line into the buffer
      val rest = region.slice(1, region.length-1).map(MutableLine(this, _)) :+ tail
      _lines.insertAll(loc.row+1, rest)
      // note the edit, and return the location at the end of the inserted region
      noteInsert(loc, loc + region)
  }

  override def delete (loc :Loc, count :Int) = {
    val dline = line(loc).delete(loc, count)
    noteDelete(loc, Seq(dline))
    dline
  }
  override def delete (start :Loc, until :Loc) =
    if (until < start) delete(until, start)
    else if (start.row == until.row) Seq(delete(start, until.col-start.col))
    else {
      val (fst, last) = (line(start), line(until))
      // replace the deleted part of the first line with the retained part of the last line
      val head = fst.replace(start, fst.length-start.col, last.slice(until.col))
      // then delete all the intervening lines
      val middle = delete(start.row+1, until.row-start.row)
      // truncate the last deleted line at the until column, otherwise we'd erroneously report the
      // retained part of that line as part of the deleted region
      val deleted = head +: middle.dropRight(1) :+ middle.last.slice(0, until.col)
      noteDelete(start, deleted)
      deleted
    }

  override def replace (loc :Loc, count :Int, line :LineV) = {
    val dline = _lines(loc.row).replace(loc, count, line)
    noteDelete(loc, Seq(dline))
    noteInsert(loc, loc + (0, line.length))
    dline
  }

  override def replace (start :Loc, until :Loc, lines :Seq[LineV]) :Loc = {
    if (until < start) replace(until, start, lines) else {
      // if this is an exact replacement, handle it specially; this is mainly for efficient undoing
      // of transforms; overkill perhaps, but whatever, it's four lines of code
      if (until == start + lines) {
        val original = new ArrayBuffer[Line]()
        onRows(start, until) { (l, s, c) => original += l.replace(s, c, lines(s.row-start.row)) }
        noteTransform(start, original)
      } else {
        delete(start, until)
        insert(start, lines)
      }
    }
  }

  override def transform (start :Loc, until :Loc, fn :Char => Char) =
    if (until < start) transform(until, start, fn)
    else {
      val original = new ArrayBuffer[Line]()
      def xf (ln :MutableLine, start :Loc, end :Int) = {
        original += ln.slice(start.col, end)
        ln.transform(fn, start, end)
      }
      if (start.row == until.row) xf(line(start), start, until.col)
      else onRows(start, until)(xf)
      noteTransform(start, original)
    }

  override def split (loc :Loc) {
    _lines.insert(loc.row+1, line(loc).split(loc))
    noteInsert(loc, loc.nextStart)
  }

  override def updateStyles (fn :Styles => Styles, start :Loc, until :Loc) {
    if (until < start) updateStyles(fn, until, start)
    else if (until > end) updateStyles(fn, start, end) // bound until into the buffer
    else if (start.row == until.row) line(start).updateStyles(fn, start, until.col)
    else onRows(start, until)(_.updateStyles(fn, _, _))
  }

  //
  // impl details

  /** Applies op to all rows from `start` up to (not including) `until`. `op` is passed `(line,
    * start, endCol)` which is adjusted properly for the first and last line. */
  private def onRows (start :Loc, until :Loc)(op :(MutableLine, Loc, Int) => Unit) {
    var loc = start
    while (loc.row < until.row) {
      val l = _lines(loc.row)
      op(l, loc, l.length)
      loc = loc.nextStart
    }
    op(_lines(loc.row), loc, until.col)
  }

  private def delete (row :Int, count :Int) :Seq[Line] =
    if (count == 0) BufferImpl.NoLines
    else {
      val deleted = _lines.slice(row, row+count)
      _lines.remove(row, count)
      // TODO: variant of slice that relinquishes the char array instead of copying it; since we're
      // giving up these lines their data can be bequeathed to immutable lines
      deleted.map(_.slice(0))
    }

  private def longest (lines :Seq[LineV], start :Int, end :Int) :Int = {
    @inline @tailrec def loop (cur :Int, max :Int, maxLen :Int) :Int =
      if (cur >= end) max
      else {
        val curLen = lines(cur).length
        if (curLen > maxLen) loop(cur+1, cur, curLen)
        else loop(cur+1, max, maxLen)
      }
    loop(start, 0, 0)
  }

  private def emit (edit :Edit) :Loc = {
    // println(edit)
    _dirty() = true
    _edited.emit(edit)
    edit.end
  }
  private def noteInsert (start :Loc, end :Loc) = {
    // shift max row, then check inserted lines for new longest line
    if (_maxRow >= start.row) _maxRow += (end.row - start.row)
    val editMaxRow = longest(_lines, start.row, end.row+1)
    if (lines(editMaxRow).length > maxLineLength) _maxRow = editMaxRow
    // TODO: if the insertion is in the max row, it might have split the max row into two non-max
    // rows, in which case we have to rescan the whole buffer; sigh...
    emit(new Insert(start, end, this))
  }
  private def noteDelete (start :Loc, deleted :Seq[Line]) = {
    val edit = new Delete(start, deleted, this)
    // if our max row was later in the buffer than the deleted region, shift it
    if (_maxRow > edit.end.row) _maxRow -= (edit.end.row - edit.start.row)
    // otherwise if our old max row was in the deleted region, rescan buffer for new longest
    else if (_maxRow >= start.row) _maxRow = longest(_lines, 0, _lines.length)
    emit(edit)
  }
  private def noteTransform (start :Loc, orig :Seq[Line]) =
    emit(new Transform(start, orig, this))

  private[impl] def noteLineStyled (loc :Loc) {
    // println(s"Styles @$loc")
    _lineStyled.emit(loc)
  }

  private val lineSep = "\n" // TODO

  override def toString () = s"[file=${file}, name=${name}, lines=${lines.size}]"
}
