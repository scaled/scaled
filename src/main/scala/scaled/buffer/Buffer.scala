//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.buffer

import java.io.{Reader, BufferedReader, File, FileReader}
import reactual.{Signal, SignalV, Value, ValueV}
import scala.collection.mutable.ArrayBuffer

/** `Buffer` related types and utilities. */
object Buffer {

  /** An event emitted when one or more lines are deleted from a buffer and replaced by one or more
    * new lines. The removed lines will have already been removed and the added lines added when
    * this edit is dispatched. */
  case class Edit (
    /** The offset (zero-based line number) in the buffer at which lines were replaced. */
    offset :Int,
    /** The number of lines that were deleted. */
    deleted :Int,
    /** The number of lines that were added. */
    added :Int,
    /** The buffer that was edited. */
    buffer :Buffer) {
    /** Extracts and returns the lines that were added. */
    def addedLines :Seq[Line] = buffer.lines.slice(offset, offset+added)
  }

  /** Reads the contents of `reader` info a buffer. Note that the caller is responsible for closing
    * the reader if necessary. */
  def apply (name :String, dir :File, reader :Reader) :Buffer = {
    val buffed = new BufferedReader(reader)
    val lines = ArrayBuffer[Line]()
    var line :String = buffed.readLine()
    while (line != null) {
      lines += new Line(line.toCharArray)
      line = buffed.readLine()
    }
    new Buffer(name, dir, lines)
  }

  /** Reads the contents of `file` into a buffer. */
  def fromFile (file :File) :Buffer = {
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

/** Models a buffer of text. This is where all the excitement happens. */
class Buffer private (initName :String, initDir :File, initLines :ArrayBuffer[Line]) {
  // TODO: character encoding
  // TODO: line endings

  private val _lines = initLines
  private val _name = Value(initName)
  private val _dir = Value(initDir)
  private val _edited = Signal[Buffer.Edit]()

  /** The name of this buffer. Tends to be the name of the file from which it was read. */
  def name :ValueV[String] = _name

  /** The directory from which this buffer was read and/or to which it was last written. */
  def dir :ValueV[File] = _dir

  /** A signal emitted when this buffer is edited (lines are inserted and/or deleted). */
  def edited :SignalV[Buffer.Edit] = _edited

  /** The lines that make up this buffer. */
  def lines :Seq[Line] = _lines

  // TODO: methods for inserting, removing and replacing lines

  override def toString () = s"[dir=${dir.get}, name=${name.get}, lines=${lines.size}]"
}
