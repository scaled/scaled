//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.File

object BufferTest {

  /** Creates a read-only buffer view for testing. */
  def bufferV (_name :String, _lines :Seq[LineV]) :BufferV = new BufferV() {
    def name = _name
    def file = new File(name)
    def mark = None
    def dirty = false
    def lines = _lines
    val maxLineLength = _lines.map(_.length).max
  }
}
