//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import reactual.Future
import scaled._

/** Provides an interface whereby project mode can initiate project compilation and display
  * compiler feedback in the appropriate buffers.
  */
abstract class Compiler {
  import Compiler._

  /** Initiates a compilation, returns a future that reports notes generated thereby. */
  def compile () :Future[Seq[Note]]

  /** Called when this compiler is no longer needed. This should terminate any external processes
    * and release any resources retained by this instance. */
  def shutdown () :Unit
}

/** Static [[Compiler]] stuffs. */
object Compiler {

  /** Models feedback from the compiler about some piece of code. */
  abstract class Note {
    /** The compilation unit to which the note applies. */
    def file :File
    /** The character offset into the file to which the note pertains. */
    def offset :Int
    /** The length of the region to which the note pertains. */
    def length :Int
    /** The message associated with the note. */
    def message :String
    /** The style class to apply to the noted region. */
    def styleClass :String
  }

  /** Represents a compiler warning. */
  case class Warning (file :File, offset :Int, length: Int, message :String) extends Note {
    def styleClass = EditorConfig.warnStyle
  }

  /** Represents a compiler error. */
  case class Error (file :File, offset :Int, length: Int, message :String) extends Note {
    def styleClass = EditorConfig.errorStyle
  }
}
