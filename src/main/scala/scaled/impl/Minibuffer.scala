//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label
import javafx.scene.layout.Region

import reactual.{Future, Promise}

import scaled._
import scaled.major.EditingMode

/** Defines the special [[BufferArea]] and [[MajorMode]] used to implement the minibuffer. These
  * all interoperate pretty closely, so we bundle them all together as one big implementation
  * detail. Maybe we'll want to tidy this up at some point and make it user extensible...
  */
object Minibuffer {

  val bufferName = "*minibuffer*"

  class Area private (ed :Editor, bv :BufferViewImpl, mode :Mode) extends BufferArea(ed, bv, mode) {
    private def this (ed :Editor, view :BufferViewImpl) = this(ed, view, new Mode(ed, view))
    def this (ed :Editor) = this(ed, new BufferViewImpl(BufferImpl.minibuffer(bufferName), 80, 1))

    /** The label that should be displayed to the left of the minibuffer area. The minibuffer will
      * manage it (setting its text when we have a prompt and making it invisible when we don't). */
    def prompt = mode.prompt

    def read (prompt :String, defval :String) :Future[String] = {
      requestFocus()
      mode.pushRead(prompt, defval)
    }

    def emitStatus (msg :String) = mode.flashStatus(msg)
  }

  private class Mode (editor :Editor, view :BufferViewImpl) extends EditingMode(editor, view) {
    override def name = "minibuffer"
    override def dispose () {}  // nothing to dispose

    override def keymap = super.keymap ++ Seq(
      "ENTER" -> "complete-read",
      "C-g"   -> "abort-read"

      // TODO: history commands

      // TODO: disable other commands (like save-buffer) or factor them out of editing mode? or
      // maybe everythign just fails because we reject minibuffer use while in the minibuffer... it
      // looks like that might be WED
    )

    val prompt = new Label("")

    case class Read (prompt :String, defval :String) { // TODO: mode
      val result = Promise[String]

      def activate () {
        setPrompt(prompt)
        view.buffer.replace(view.buffer.start, view.buffer.end, _curval)
      }

      def deactivate () {
        _curval = view.buffer.delete(view.buffer.start, view.buffer.end)
        view.point = view.buffer.start
      }

      def succeed () = result.succeed(_curval.head.asString)
      def abort () = result.fail(new Exception("Aborted"))

      private var _curval :Seq[Line] = Seq(new Line(defval))
    }
    var _reads :List[Read] = Nil

    def setPrompt (text :String) {
      prompt.setText(text)
      prompt.setVisible(!text.isEmpty)
    }

    def flashStatus (msg :String) :Unit = println(s"Status: $msg")

    def pushRead (prompt :String, defval :String) :Future[String] = {
      val read = Read(prompt, defval)
      _reads = read :: _reads
      read.activate()
      read.result
    }

    @Fn("Completes the current minibuffer read.")
    def completeRead () = _reads match {
      case Nil    => flashStatus("Minibuffer read completed but have no pending read?!")
      case h :: t =>
        h.deactivate()
        popRead(t)
        h.succeed()
    }

    @Fn("Aborts the current minibuffer read.")
    def abortRead () = _reads match {
      case Nil    => flashStatus("Minibuffer read aborted but have no pending read?!")
      case h :: t =>
        h.deactivate()
        popRead(t)
        flashStatus("Quit")
        h.abort()
    }

    private def popRead (reads :List[Read]) = reads match {
      case Nil =>
        _reads = Nil
        setPrompt("")
      case h :: t =>
        _reads = reads
        h.activate()
    }
  }
}
