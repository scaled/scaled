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

  def create (editor :EditorPane) :(Label, Area) = {
    val prompt = new Label()
    prompt.getStyleClass.add("prompt")
    val view = new BufferViewImpl(editor, BufferImpl.scratch(bufName), 80, 1)
    val disp = new DispatcherImpl(editor, view) {
      override def createMode () = new Mode(editor, view, this, prompt)
    }
    val area = new Area(editor, view, disp)
    prompt.backgroundProperty.bind(area.backgroundProperty)
    (prompt, area)
  }

  class Area (editor :EditorPane, view :BufferViewImpl, disp :DispatcherImpl)
      extends BufferArea(editor, view, disp) {
    private[this] val mode = disp.major.asInstanceOf[Mode]

    def read (prompt :String, defval :String, completer :String => Set[String]) :Future[String] =
      mode.read(prompt, defval, completer)
    def readYN (prompt :String) :Future[Boolean] = mode.readYN(prompt)
    def emitStatus (msg :String) = mode.flashStatus(msg)
    def clearStatus () = mode.clearStatus()
  }

  private class Mode (editor :EditorPane, view :BufferViewImpl, disp :DispatcherImpl, prompt :Label)
      extends EditingMode(editor, editor.config, view, disp) {
    override def name = "minibuffer"
    override def dispose () {}  // nothing to dispose

    override def defaultFn = Some("default-insert")

    override def keymap = super.keymap ++ Seq(
      "ENTER" -> "commit-read",
      "TAB"   -> "complete",
      "C-g"   -> "abort-read"

      // TODO: history commands

      // TODO: disable other commands (like save-buffer) or factor them out of editing mode? or
      // maybe everythign just fails because we reject minibuffer use while in the minibuffer... it
      // looks like that might be WED
    )

    //
    // "public" API

    def flashStatus (msg :String) {
      if (_read != null) _read.deactivate()
      setPrompt("")
      setBuffer(msg)
      view.popup.clear()
      // TODO: set a timer to clear this status if we have other reads
    }
    def clearStatus () {
      if (_read == null) clear()
      else if (!_read.active) _read.activate()
    }
    def read (prompt :String, defval :String, completer :String => Set[String]) :Future[String] =
      setRead(new ReadString(prompt, defval, completer))
    def readYN (prompt :String) :Future[Boolean] =
      setRead(new YorNRead(prompt))

    //
    // Fns

    @Fn("Handles insertion of typed characters into minibuffer.")
    def defaultInsert (typed :String) {
      if (_read == null) selfInsertCommand(typed)
      else _read.insert(typed)
    }

    @Fn("Commits the current minibuffer read with its current contents.")
    def commitRead () {
      if (_read == null) flashStatus("Minibuffer read completed but have no active read?!")
      else clearRead().commit()
    }

    @Fn("Aborts the current minibuffer read.")
    def abortRead () {
      if (_read == null) flashStatus("Minibuffer read aborted but have no active read?!")
      else clearRead().abort()
    }

    @Fn("Completes the minibuffer contents as much as possible.")
    def complete () {
      if (_read == null) flashStatus("Minibuffer complete requested but have no active read?!")
      else _read.complete()
    }

    //
    // Implementation details

    private abstract class Read[T] (prompt :String, defval :String) {
      val result = Promise[T]

      def active :Boolean = _curval == null
      def activate () {
        assert(!active)
        setPrompt(prompt)
        setBuffer(_curval)
        _curval = null
      }
      def deactivate () {
        assert(active)
        _curval = view.buffer.delete(view.buffer.start, view.buffer.end)
      }

      def insert (typed :String) :Unit
      def complete () :Unit

      def commit () :Unit
      def abort () {
        flashStatus("Quit")
        result.fail(new Exception("Aborted"))
      }

      def curval :String = mkString(_curval)
      protected def mkString (lines :Seq[Line]) = lines.map(_.asString).mkString("\n")

      private[this] var _curval = Seq(new Line(defval))
    }

    private class ReadString (prompt :String, defval :String, completer :String => Set[String])
        extends Read[String](prompt, defval) {

      override def insert (typed :String) = selfInsertCommand(typed)

      override def complete () {
        val current = mkString(view.buffer.region(view.buffer.start, view.buffer.end))
        val comps = completer(current)
        if (comps.isEmpty) {
          view.popup() = Popup(Seq("No match."), Popup.UpRight(0, 0), true)
        }
        else if (comps.size == 1) {
          view.popup.clear()
          setBuffer(comps.head)
        }
        else {
          view.popup() = Popup(comps.toSeq.sorted, Popup.UpRight(0, 0), false)
          val pre = longestPrefix(comps)
          if (pre != current) setBuffer(pre)
        }
      }

      override def commit () = result.succeed(curval)

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
    }

    // TODO: in the distant future we'll want to i18n this somehow...
    private class YorNRead (prompt :String) extends Read[Boolean](prompt + " (y or n)", "") {
      override def insert (typed :String) = typed match {
        // TODO: having to clearRead() here feels fragile, is there a better way?
        case "y" => clearRead() ; result.succeed(true)
        case "n" => clearRead() ; result.succeed(false)
        case _   => setPrompt(s"Please answer y or n. $prompt")
      }
      override def complete () = insert("	") // ditto
      override def commit () = insert("\n") // will trigger 'need y or n'
    }

    private[this] var _read :Read[_] = _

    private def setBuffer (text :String) :Unit = setBuffer(Seq(new Line(text)))
    private def setBuffer (text :Seq[Line]) {
      view.buffer.replace(view.buffer.start, view.buffer.end, text)
      view.point = view.buffer.end
    }

    private def setPrompt (text :String) {
      prompt.setText(text)
      prompt.setManaged(text != "")
    }

    private def setRead[T] (read :Read[T]) :Future[T] = {
      if (_read == null) {
        _read = read
        read.activate()
        read.result
      } else {
        flashStatus("Command attempted to use minibuffer while in minibuffer.")
        Future.failure(new Exception("minibuffer in use"))
      }

    }

    private def clearRead () = {
      val read = _read
      _read = null
      read.deactivate()
      clear()
      read
    }

    private def clear () {
      assert(_read == null)
      setPrompt("")
      setBuffer("")
      view.popup.clear() // clear any active popup
    }
  }
}
