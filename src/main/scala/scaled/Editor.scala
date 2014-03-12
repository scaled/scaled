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
  def emitStatus (msg :String) :Unit

  /** Clears any lingering status message. A status message usually remains visible until the user
    * types the next key, so this allows any buffer which receives key input to clear the last
    * status message. */
  def clearStatus () :Unit

  /** Prompts the user to input a string via the minibuffer.
    *
    * @param prompt the text to display to the left of the minibuffer when requesting input.
    * @param defval the default value with which to populate the minibuffer.
    * @param completer a completion function used to generate completions when the user presses TAB.
    *                  It will be passed the current minibuffer contents (the prefix) and should
    *                  return the set of all valid completions. The completions should be the
    *                  entire values, not suffixes to be appended to the prefix.
    */
  def miniRead (prompt :String, defval :String, completer :String => Set[String]) :Future[String]
}
