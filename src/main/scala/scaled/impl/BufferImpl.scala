//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import java.io.{Reader, BufferedReader, File, FileReader}
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
  def apply (name :String, dir :File, reader :Reader) :BufferImpl = {
    val buffed = new BufferedReader(reader)
    val lines = ArrayBuffer[Array[Char]]()
    var line :String = buffed.readLine()
    while (line != null) {
      lines += line.toCharArray
      line = buffed.readLine()
    }
    // TEMP: tack a blank line on the end to simulate a trailing line sep
    lines += LineImpl.NoChars
    new BufferImpl(name, dir, lines)
  }

  /** Reads the contents of `file` into a buffer. */
  def fromFile (file :File) :BufferImpl = {
    // TODO: use a CharSetDecoder and ByteBuffer + CharBuffer to read things ourselves, and track
    // where and what the line separators are, and whether there's a trailing line sep
    val reader = new FileReader(file)
    try {
      apply(file.getName, file.getParentFile, reader)
    } finally {
      reader.close
    }
  }

  /** An empty line sequence used for edits that delete no lines. */
  private final val NoLines = Seq[Line]()
}

/** Implements [Buffer] and [RBuffer]. This is where all the excitement happens. */
class BufferImpl private (
  initName :String, initDir :File, initLines :ArrayBuffer[Array[Char]]
) extends RBuffer {
  // TODO: character encoding
  // TODO: line endings

  private val _lines = initLines.map(l => new LineImpl(l, this))
  private val _name = Value(initName)
  private val _dir = Value(initDir)
  private val _mark = Value(None :Option[Loc])
  private val _edited = Signal[Buffer.Edit]()
  private val _lineEdited = Signal[Line.Edit]()

  //
  // from Buffer and RBuffer API

  override def nameV = _name
  override def dirV = _dir
  override def markV = _mark
  override def mark_= (loc :Loc) = _mark.update(Some(bound(loc)))
  override def clearMark () = _mark.update(None)
  override def edited = _edited
  override def lineEdited = _lineEdited
  override def lines = _lines
  // refine return type to ease life for internal friends
  override def line (idx :Int) :LineImpl = _lines(idx)
  override def line (loc :Loc) :LineImpl = line(loc.row)

  override def region (start :Loc, until :Loc) = if (until < start) region(until, start) else {
    if (start.row == until.row) Seq(line(start).slice(start.col, until.col))
    else {
      val middle = lines.slice(start.row+1, until.row).map(_.copy)
      val s = line(start)
      s.slice(start.col) +=: middle
      middle += line(until).slice(0, until.col)
      middle.toSeq
    }
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

  override def insert (idx :Int, lines :Seq[Array[Char]]) {
    _lines.insert(idx, lines.map(cs => new LineImpl(cs, this)) :_*)
    _edited.emit(Buffer.Edit(idx, BufferImpl.NoLines, lines.length, this))
  }

  override def insert (loc :Loc, region :Seq[LineV]) = region.size match {
    case 0 => // nada
    case 1 =>
      _lines(loc.row).insert(loc.col, region.head)
    case _ =>
      val tail = _lines(loc.row).split(loc.col)
      _lines(loc.row).append(region.head)
      _lines.insertAll(loc.row+1, region.drop(1).map(_.asInstanceOf[LineImpl].copy()))
      _lines(loc.row+region.size-1).append(tail)
  }

  override def delete (idx :Int, count :Int) {
    _edited.emit(Buffer.Edit(idx, deleteLines(idx, count), 0, this))
  }

  override def split (idx :Int, pos :Int) {
    _lines.insert(idx+1, _lines(idx).split(pos))
    _edited.emit(Buffer.Edit(idx+1, BufferImpl.NoLines, 1, this))
  }

  override def join (idx :Int) {
    val suff = _lines(idx+1)
    delete(idx+1, 1)
    _lines(idx).append(suff)
  }

  override private[scaled] def undo (edit :Buffer.Edit) {
    assert(edit.buffer == this)
    val undoneLines = deleteLines(edit.offset, edit.added)
    _lines.insert(edit.offset, edit.deletedLines.map(_.asInstanceOf[LineImpl]) :_*)
    _edited.emit(Buffer.Edit(edit.offset, undoneLines, edit.deleted, this))
  }

  private def deleteLines (idx :Int, count :Int) :Seq[Line] =
    if (count == 0) BufferImpl.NoLines
    else {
      val deleted = _lines.slice(idx, idx+count)
      _lines.remove(idx, count)
      deleted
    }

  //
  // impl details

  private[impl] def noteEdited (edit :Line.Edit) = _lineEdited.emit(edit)

  private val lineSep = "\n" // TODO

  override def toString () = s"[dir=${dir}, name=${name}, lines=${lines.size}]"
}
