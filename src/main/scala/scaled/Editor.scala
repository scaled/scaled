//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import reactual.Future

/** Provides access to certain global functionality that doesn't fit nicely elsewhere. */
trait Editor {

  /** Terminates the editor.
    * @param code the status code to report to the operating system.
    */
  def exit (code :Int) :Unit

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** The kill ring shared by all buffers in this editor. */
  def killRing :KillRing

  /** Briefly displays a status message in the minibuffer. The status message will also be appeneded
    * to an editor-wide messages list. */
  def emitStatus (msg :String)

  /** Prompts the user to input a string via the minibuffer. */
  def miniRead (prompt :String, defval :String) :Future[String]
  // TODO: miniRead variant that takes a tab-completer?
}
