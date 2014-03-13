//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label

import scala.annotation.tailrec

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
    prompt.getStyleClass.add("prompt")
    val view = new BufferViewImpl(editor, BufferImpl.scratch(bufName), 80, 1)
    val disp = new DispatcherImpl(editor, view) {
      override def createMode () = new Mode(editor, view, this, prompt)
    }
    (prompt, new Area(editor, view, disp))
  }

  class Area (editor :Editor, view :BufferViewImpl, disp :DispatcherImpl)
      extends BufferArea(editor, view, disp) {
    private[this] val mode = disp.major.asInstanceOf[Mode]

    def read (prompt :String, defval :String, completer :String => Set[String]) :Future[String] = {
      requestFocus()
      mode.pushRead(prompt, defval, completer)
    }
    def emitStatus (msg :String) = mode.flashStatus(msg)
    def clearStatus () = mode.clearStatus()
  }

  private class Mode (editor :Editor, view :BufferViewImpl, disp :DispatcherImpl, prompt :Label)
      extends EditingMode(editor, view, disp) {
    override def name = "minibuffer"
    override def dispose () {}  // nothing to dispose

    override def keymap = super.keymap ++ Seq(
      "ENTER" -> "succeed-read",
      "C-g"   -> "abort-read",
      "TAB"   -> "complete"

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
    def pushRead (prompt :String, defval :String, completer :String => Set[String]) :Future[String] =
      pushRead(new Read(prompt, defval, completer)).result

    //
    // Fns

    @Fn("Succeeds the current minibuffer read with its current contents.")
    def succeedRead () {
      if (_reads.isEmpty) flashStatus("Minibuffer read completed but have no active read?!")
      else popRead().succeed()
    }

    @Fn("Aborts the current minibuffer read.")
    def abortRead () {
      if (_reads.isEmpty) flashStatus("Minibuffer read aborted but have no active read?!")
      else popRead().abort()
    }

    @Fn("Completes the minibuffer contents as much as possible.")
    def complete () {
      if (_reads.isEmpty) flashStatus("Minibuffer complete requested but have no active read?!")
      else _reads.head.complete()
    }

    //
    // Implementation details

    private class Read (prompt :String, defval :String, completer :String => Set[String]) {
      val result = Promise[String]

      def activate () {
        setPrompt(prompt)
        setBuffer(_curval)
      }
      def deactivate () {
        _curval = view.buffer.delete(view.buffer.start, view.buffer.end)
        view.point = view.buffer.start
      }

      def complete () {
        val current = mkString(view.buffer.region(view.buffer.start, view.buffer.end))
        val comps = completer(current)
        if (comps.isEmpty) flashStatus("No match.")
        else if (comps.size == 1) setBuffer(comps.head)
        else longestPrefix(comps) match {
          case pre if (pre == "" || pre == current)  =>
            flashStatus(s"TODO: show ${comps.size} matches")
            println(s"Matches $comps")
          case pre => setBuffer(pre)
        }
      }

      def setBuffer (text :String) :Unit = setBuffer(Seq(new Line(text)))
      def setBuffer (text :Seq[Line]) {
        view.buffer.replace(view.buffer.start, view.buffer.end, text)
        view.point = view.buffer.end
      }

      def succeed () = result.succeed(curval)
      def abort () {
        flashStatus("Quit")
        result.fail(new Exception("Aborted"))
      }

      def curval :String = mkString(_curval)
      private def mkString (lines :Seq[Line]) = lines.map(_.asString).mkString("\n")

      private def sharedPrefix (a :String, b :String) = if (b startsWith a) a else {
        val buf = new StringBuilder
        @tailrec def loop (ii :Int) {
          if (ii < a.length && ii < b.length && a.charAt(ii) == b.charAt(ii)) {
            buf.append(a.charAt(ii))
            loop(ii+1)
          }
        }
        loop(0)
        buf.toString
      }
      private def longestPrefix (comps :Set[String]) = comps reduce sharedPrefix

      private[this] var _curval = Seq(new Line(defval))
    }
    private class Status (msg :String) extends Read("", msg, null)
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
