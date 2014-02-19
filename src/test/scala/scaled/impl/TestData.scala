//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.{File, StringReader}

/** Helper methods for creating test instances of things. */
object TestData {

  /** Creates a test buffer. For testing! */
  def buffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))
}
