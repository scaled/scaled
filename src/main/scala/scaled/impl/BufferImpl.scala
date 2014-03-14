//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import java.io.{Reader, BufferedReader, File, FileReader, StringReader}
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
    // TODO: use a CharSetDecoder and ByteBuffer + CharBuffer to read things ourselves, and track
    // where and what the line separators are, and whether there's a trailing line sep
    val reader = new FileReader(file)
    try {
      apply(file.getName, file, reader)
    } finally {
      reader.close
    }
  }

  /** Returns a blank buffer to be used by scratch views (e.g. the minibuffer). */
  def scratch (name :String) :BufferImpl = apply(name, new File(""), new StringReader(""))

  /** An empty line sequence used for edits that delete no lines. */
  private final val NoLines = Seq[Line]()
}

/** Implements [Buffer] and [RBuffer]. This is where all the excitement happens. */
class BufferImpl private (
  initName :String, initFile :File, initLines :ArrayBuffer[Array[Char]]
) extends RBuffer {
  // TODO: character encoding
  // TODO: line endings

  private val _lines = initLines.map(new MutableLine(this, _))
  private val _name = Value(initName)
  private val _file = Value(initFile)
  private val _mark = Value(None :Option[Loc])
  private val _edited = Signal[Buffer.Edit]()
  private val _lineEdited = Signal[Line.Edit]()

  val undoStack = new UndoStack(this)

  //
  // from Buffer and RBuffer API

  override def nameV = _name
  override def fileV = _file
  override def markV = _mark
  override def mark_= (loc :Loc) = _mark.update(Some(bound(loc)))
  override def clearMark () = _mark.update(None)
  override def undoer = undoStack
  override def edited = _edited
  override def lineEdited = _lineEdited
  override def lines = _lines
  // refine return type to ease life for internal friends
  override def line (idx :Int) :MutableLine = _lines(idx)
  override def line (loc :Loc) :MutableLine = _lines(loc.row)

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

  override def region (start :Loc, until :Loc) = if (until < start) region(until, start) else {
    if (start.row == until.row) Seq(line(start).slice(start.col, until.col))
    else {
      val middle = lines.slice(start.row+1, until.row).map(_.slice(0))
      val s = line(start)
      s.slice(start.col) +=: middle
      middle += _lines(until.row).slice(0, until.col)
      middle.toSeq
    }
  }

  override def insert (loc :Loc, c :Char) = _lines(loc.row).insert(loc, c)
  override def insert (loc :Loc, line :LineV) = _lines(loc.row).insert(loc, line)
  override def insert (loc :Loc, region :Seq[LineV]) = region.size match {
    case 0 => loc
    case 1 =>
      line(loc).insert(loc, region.head)
      // no new lines inserted, so no need to emit a buffer edit
    case _ =>
      // split the first line in twain
      val tail = line(loc).split(loc)
      // merge the first half of the split line with the first line of the region
      line(loc).append(loc, region.head)
      // merge the last line of the region with the second half of the split line
      tail.insertSilent(0, region.last, 0, region.last.length)
      // finally add the middle lines (unmodified) and the merged last line into the buffer
      val rest = region.slice(1, region.length-1).map(new MutableLine(this, _)) :+ tail
      _lines.insertAll(loc.row+1, rest)
      _edited.emit(Buffer.Edit(loc.row+1, BufferImpl.NoLines, rest.length, this))
      // return the location at the end of the inserted region
      Loc(loc.row+region.length-1, region.last.length)
  }

  override def delete (loc :Loc, count :Int) = line(loc).delete(loc, count)
  override def delete (start :Loc, until :Loc) =
    if (until < start) delete(until, start)
    else if (start.row == until.row) Seq(line(start).delete(start, until.col-start.col))
    else {
      val (fst, last) = (line(start), line(until))
      // replace the deleted part of the first line with the retained part of the last line
      val head = fst.replace(start, fst.length-start.col, last.slice(until.col))
      // then delete all the intervening lines
      val deleted = deleteEmit(start.row+1, until.row-start.row)
      // truncate the last deleted line at the until column, otherwise we'd erroneously report the
      // retained part of that line as part of the deleted region
      head +: deleted.dropRight(1) :+ deleted.last.slice(0, until.col)
    }

  override def replace (loc :Loc, delete :Int, line :Line) =
    _lines(loc.row).replace(loc, delete, line)

  override def split (loc :Loc) {
    _lines.insert(loc.row+1, line(loc).split(loc))
    _edited.emit(Buffer.Edit(loc.row+1, BufferImpl.NoLines, 1, this))
  }

  override private[scaled] def undo (edit :Buffer.Edit) {
    assert(edit.buffer == this)
    val undoneInserts = delete(edit.offset, edit.added)
    _lines.insertAll(edit.offset, edit.deletedLines.map(new MutableLine(this, _)))
    _edited.emit(Buffer.Edit(edit.offset, undoneInserts, edit.deleted, this))
  }

  //
  // impl details

  private def delete (row :Int, count :Int) :Seq[Line] =
    if (count == 0) BufferImpl.NoLines
    else {
      val deleted = _lines.slice(row, row+count)
      _lines.remove(row, count)
      // TODO: variant of slice that relinquishes the char array instead of copying it; since we're
      // giving up these lines their data can be bequeathed to immutable lines
      deleted.map(_.slice(0))
    }

  private def deleteEmit (row :Int, count :Int) :Seq[Line] = {
    val deleted = delete(row, count)
    if (!deleted.isEmpty) _edited.emit(Buffer.Edit(row, deleted, 0, this))
    deleted
  }

  private[impl] def noteLineEdited (loc :Loc, deleted :Line, added :Int) =
    _lineEdited.emit(Line.Edit(loc, deleted, added, this))

  private val lineSep = "\n" // TODO

  override def toString () = s"[file=${file}, name=${name}, lines=${lines.size}]"
}
