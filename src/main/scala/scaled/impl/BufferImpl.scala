//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.{Reader, BufferedReader, File, FileReader}
import reactual.{Signal, SignalV, Value, ValueV}
import scala.collection.mutable.ArrayBuffer

import scaled.{Buffer, RBuffer}

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
    val lines = ArrayBuffer[LineImpl]()
    var line :String = buffed.readLine()
    while (line != null) {
      lines += new LineImpl(line.toCharArray)
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
  initName :String, initDir :File, initLines :ArrayBuffer[LineImpl]
) extends RBuffer {
  // TODO: character encoding
  // TODO: line endings

  private val _lines = initLines
  private val _name = Value(initName)
  private val _dir = Value(initDir)
  private val _edited = Signal[Buffer.Edit]()

  def name = nameV.get
  def nameV = _name
  def dir = dirV.get
  def dirV = _dir
  def edited = _edited
  def line (idx :Int) = _lines(idx)
  def lines = _lines

  override def toString () = s"[dir=${dir}, name=${name}, lines=${lines.size}]"
}
