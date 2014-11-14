//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scala.collection.mutable.{Set => MSet}

import scaled._
import scaled.util.{Behavior, Chars}

object WhitespaceConfig extends Config.Defs {

  @Var("If true, trailing whitespace will be highlighted.")
  val showTrailingWhitespace = key(true)

  @Var("If true, trailing whitespace will be trimmed from a line when a newline is inserted.")
  val trimTrailingWhitespace = key(true)

  @Var("""If true and the buffer lacks a trailing newline, a newline will be appended to the buffer
          before it is saved.""")
  val requireTrailingNewline = key(true)

  /** The CSS style applied to trailing whitespace characters. */
  val trailingStyle = "whitespaceTrailingFace"
}

@Minor(name="whitespace", tags=Array("text", "code"),
       desc="""Provides whitespace manipulation fns; highlights undesirable whitespace.""")
class WhitespaceMode (env :Env) extends MinorMode(env) {
  import WhitespaceConfig._
  import Chars._

  addBehavior(showTrailingWhitespace, new Behavior() {
    private val _rethinkLines = MSet[Int]()

    override protected def activate () {
      // respond to buffer edits
      note(buffer.edited onValue { edit =>
        queueRethink(edit.start.row until edit.end.row :_*)
      })
      // when the point moves, the line it left may now need highlighting and the line it moves to
      // may no longer need highlighting
      note(view.point onChange { (p, op) => queueRethink(op.row, p.row) })
      // note existing trailing whitespace
      0 until buffer.lines.size foreach tagTrailingWhitespace
      // TODO: defer marking trailing whitespace on non-visible lines until they're scrolled into
      // view, we can probably do this entirely in client code using RBufferView.scrollTop and
      // RBufferView.heightV; encapsulate it in a Colorizer helper class?
    }

    override protected def didDeactivate (bufferDisposing :Boolean) {
      if (!bufferDisposing) buffer.removeStyle(trailingStyle, buffer.start, buffer.end)
    }

    private def queueRethink (row :Int*) {
      val takeAction = _rethinkLines.isEmpty
      _rethinkLines ++= row
      if (takeAction) env.exec runOnUI rethink
    }

    private def rethink () {
      _rethinkLines foreach tagTrailingWhitespace
      _rethinkLines.clear()
    }

    // we might have queued a line for a rethink that then disappeared, so be sure that the line
    // we're rethinking is still part of the buffer
    private val tagTrailingWhitespace = (ii :Int) => if (ii < buffer.lines.length) {
      val line = buffer.lines(ii)
      val limit = if (view.point().row == ii) view.point().col else 0
      @tailrec def seek (col :Int) :Int = {
        if (col == limit || isNotWhitespace(line.charAt(col-1))) col
        else seek(col-1)
      }
      val last = line.length
      val first = seek(last)
      val floc = Loc(ii, first)
      if (first > 0) buffer.removeStyle(trailingStyle, Loc(ii, 0), floc)
      if (first < last) buffer.addStyle(trailingStyle, floc, Loc(ii, last))
    }
  })

  addBehavior(trimTrailingWhitespace, new Behavior() {
    private val _trimLines = MSet[Int]()

    override protected def activate () {
      // note lines to be trimmed when we see edits of the appropriate form
      note(buffer.edited onValue { _ match {
        case Buffer.Insert(start, end) =>
          if (end.col == 0 && end.row == start.row + 1) _trimLines += start.row
        case _ => // ignore
      }})
      // trim the to-be-trimmed lines in the did-invoke hook; we cannot modify the buffer when
      // responding to buffer modifications, but doing it in did-invoke ensures that our changes
      // are bundled up with the original fns with regard to the undo stack
      note(disp.didInvoke onEmit {
        // we don't trim whitespace from the row that contains the point;
        // the user is working on that row and that whitespace might be meaningful
        val p = view.point()
        _trimLines foreach { row => if (p.row != row) trimTrailingWhitespaceAt(row) }
        _trimLines.clear()
      })
    }
  })

  addBehavior(requireTrailingNewline, new Behavior() {
    override protected def activate () {
      note(buffer.willSave onValue { buf =>
        if (buf.lines.last.length > 0) buf.split(buf.end)
      })
    }
  })

  override def keymap = super.keymap.
    bind("trim-buffer-trailing-whitespace", "C-M-]");
  override def configDefs = WhitespaceConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/whitespace.css") :: super.stylesheets

  def trimTrailingWhitespaceAt (row :Int) {
    val line = buffer.line(row) ; val len = line.length ; val last = len-1
    var pos = last ; while (pos >= 0 && isWhitespace(line.charAt(pos))) pos -= 1
    if (pos < last) buffer.delete(Loc(row, pos+1), Loc(row, len))
  }

  @Fn("Trims trailing whitespace from the line at the point.")
  def trimLineTrailingWhitespace () {
    trimTrailingWhitespaceAt(view.point().row)
  }

  @Fn("Trims trailing whitespace from all lines in the buffer.")
  def trimBufferTrailingWhitespace () {
    0 until buffer.lines.length foreach(trimTrailingWhitespaceAt)
  }
}
