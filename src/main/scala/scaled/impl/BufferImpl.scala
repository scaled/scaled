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
    new BufferImpl(name, dir, lines)
  }

  /** Reads the contents of `file` into a buffer. */
  def fromFile (file :File) :BufferImpl = {
    // TODO: rewrite all this to use a mmap'd file and scan for CR/LF ourselves and construct the
    // lines directly from the mmap'd file data, which will reduce expense to one read to find
    // CR/LF and one read to copy the data into one array per line
    val reader = new FileReader(file)
    try {
      apply(file.getName, file.getParentFile, reader)
    } finally {
      reader.close
    }
  }
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
  private val _edited = Signal[Buffer.Edit]()
  private val _lineEdited = Signal[Line.Edit]()

  override def name = nameV.get
  override def nameV = _name
  override def dir = dirV.get
  override def dirV = _dir
  override def edited = _edited
  override def lineEdited = _lineEdited
  override def line (idx :Int) = _lines(idx)
  override def lines = _lines

  override def loc (offset :Int) = {
    assert(offset >= 0)
    def seek (off :Int, idx :Int) :Loc = {
      // if we've spilled past the end of our buffer, trim offset to fit and return a location one
      // line past the last line of our buffer; TODO: what if we don't force trailing newlines?
      if (idx >= _lines.length) Loc(_lines.length, 0)
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

  private[impl] def noteEdited (edit :Line.Edit) = _lineEdited.emit(edit)

  private val lineSep = "\n" // TODO

  override def toString () = s"[dir=${dir}, name=${name}, lines=${lines.size}]"
}
