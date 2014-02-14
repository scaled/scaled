//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Provides access to certain global functionality that doesn't fit nicely elsewhere. */
trait Editor {

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** Terminates the editor.
    * @param code the status code to report to the operating system.
    */
  def exit (code :Int) :Unit
}
