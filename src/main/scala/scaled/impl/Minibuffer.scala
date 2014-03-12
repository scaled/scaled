//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label

import reactual.{Future, Promise}

import scaled._
import scaled.major.EditingMode

/** Defines the special [[BufferArea]] and [[MajorMode]] used to implement the minibuffer. These
  * all interoperate pretty closely, so we bundle them all together as one big implementation
  * detail. Maybe we'll want to tidy this up at some point and make it user extensible...
  */
object Minibuffer {

  val bufName = "*minibuffer*"

  def create (editor :Editor) :(Label, Area) = {
    val prompt = new Label()
    val view = new BufferViewImpl(editor, BufferImpl.minibuffer(bufName), 80, 1)
    val mode = new Mode(editor, view, prompt)
    (prompt, new Area(editor, view, mode))
  }

  class Area (ed :Editor, view :BufferViewImpl, mode :Mode) extends BufferArea(ed, view, mode) {
    def read (prompt :String, defval :String) :Future[String] = {
      requestFocus()
      mode.pushRead(prompt, defval)
    }
    def emitStatus (msg :String) = mode.flashStatus(msg)
    def clearStatus () = mode.clearStatus()
  }

  private class Mode (ed :Editor, bv :BufferViewImpl, prompt :Label) extends EditingMode(ed, bv) {
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

    //
    // "public" API

    def flashStatus (msg :String) {
      clearStatus()
      pushRead(new Status(msg))
      // TODO: set a timer to clear this status if we have other reads
    }
    def clearStatus () {
      if (!_reads.isEmpty && _reads.head.isInstanceOf[Status]) popRead()
    }
    def pushRead (prompt :String, defval :String) :Future[String] =
      pushRead(new Read(prompt, defval)).result

    //
    // Fns

    @Fn("Completes the current minibuffer read.")
    def completeRead () {
      if (_reads.isEmpty) flashStatus("Minibuffer read completed but have no pending read?!")
      else popRead().succeed()
    }

    @Fn("Aborts the current minibuffer read.")
    def abortRead () {
      if (_reads.isEmpty) flashStatus("Minibuffer read aborted but have no pending read?!")
      else popRead().abort()
    }

    //
    // Implementation details

    private class Read (prompt :String, defval :String) { // TODO: mode
      val result = Promise[String]

      def activate () {
        setPrompt(prompt)
        bv.buffer.replace(bv.buffer.start, bv.buffer.end, _curval)
        bv.point = bv.buffer.end
      }
      def deactivate () {
        _curval = bv.buffer.delete(bv.buffer.start, bv.buffer.end)
        bv.point = bv.buffer.start
      }

      def succeed () = result.succeed(curval)
      def abort () {
        flashStatus("Quit")
        result.fail(new Exception("Aborted"))
      }

      def curval :String = _curval.map(_.asString).mkString("\n")

      private[this] var _curval = Seq(new Line(defval))
    }
    private class Status (msg :String) extends Read("", msg)
    private[this] var _reads :List[Read] = Nil

    private def pushRead (read :Read) :Read = {
      if (!_reads.isEmpty) _reads.head.deactivate()
      _reads = read :: _reads
      read.activate()
      read
    }

    private def popRead () :Read = {
      val head = _reads.head ; val tail = _reads.tail
      head.deactivate()
      _reads = tail
      if (tail.isEmpty) setPrompt("")
      else tail.head.activate()
      head
    }

    private def setPrompt (text :String) {
      prompt.setText(text)
      prompt.setVisible(!text.isEmpty)
    }
  }
}
