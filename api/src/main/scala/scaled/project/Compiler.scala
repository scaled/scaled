//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import reactual.Future
import scaled._

/** Provides an interface whereby project mode can initiate project compilation and display
  * compiler feedback in the appropriate buffers.
  */
abstract class Compiler {
  import Compiler._

  /** Initiates a compilation, sends output to `buffer`, returns a future that provides a summary
    * string to be reported to the user when the compile completes. */
  def compile (buffer :Buffer) :Future[String]

  /** Scans `buffer` from `start` to find the next error.
    *
    * @return a tuple containing an error, and the location at which to search for subsequent
    * errors, or `None` if no next error was found.
    */
  def nextError (buffer :Buffer, start :Loc) :Option[(Error,Loc)]

  /** Called when this compiler is no longer needed. This should terminate any external processes
    * and release any resources retained by this instance. */
  def shutdown () :Unit
}

/** Static [[Compiler]] stuffs. */
object Compiler {

  /** Represents a compilation error extracted from a buffer.
    * @param path the path to the compilation unit to which the error refers
    * @param loc the location of the error in that compilation unit
    * @param descrip the description of the error provided by the compiler
    */
  case class Error (path :String, loc :Loc, descrip :String)
}
