//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label
import javafx.scene.layout.Region

import reactual.Future

import scaled._
import scaled.major.TextMode

/** Defines the special [[BufferArea]] and [[MajorMode]] used to implement the minibuffer. These
  * all interoperate pretty closely, so we bundle them all together as one big implementation
  * detail. Maybe we'll want to tidy this up at some point and make it user extensible...
  */
object Minibuffer {

  val bufferName = "*minibuffer*"

  class Area private (editor :Editor, view :BufferViewImpl)
      extends BufferArea(editor, view, new Mode(editor, view)) {

    def this (editor :Editor) = this(
      editor, new BufferViewImpl(BufferImpl.minibuffer(bufferName), 80, 1))

    /** The label that should be displayed to the left of the minibuffer area. The minibuffer will
      * manage it (setting its text when we have a prompt and making it invisible when we don't). */
    val prompt = new Label("")

    // setEditable(false)
    setFocusTraversable(false)

    def read (prompt :String, defval :String) :Future[String] = {
      Future.failure(new Exception("TODO"))
    }

    def emitStatus (msg :String) {
    }
  }

  private class Mode (editor :Editor, view :BufferViewImpl) extends TextMode(editor, view) {
    // TODO
  }
}
