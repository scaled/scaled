//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.minor

import scala.annotation.tailrec
import scala.collection.mutable.{Set => MSet}

import scaled._
import scaled.major.EditingMode
import scaled.util.{Behavior, Chars}

object WhitespaceConfig extends Config.Defs {

  @Var("If true, trailing whitespace will be highlighted.")
  val showTrailingWhitespace = key(true)

  /** The CSS style applied to trailing whitespace characters. */
  val trailingStyle = "whitespaceTrailingFace"
}

@Minor(name="whitespace",
       tags=Array("text", "code"),
       desc="""A minor mode that provides whitespace manipulation fns and can highlight
               undesirable whitespace.""")
class WhitespaceMode (env :Env, major :EditingMode) extends MinorMode(env) {
  import WhitespaceConfig._
  import Chars._

  val trailingWhitespacer = new Behavior() {
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

    override protected def didDeactivate () {
      buffer.removeStyle(trailingStyle, buffer.start, buffer.end)
    }

    private def queueRethink (row :Int*) {
      val takeAction = _rethinkLines.isEmpty
      _rethinkLines ++= row
      if (takeAction) editor defer rethink
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
  }
  config.value(showTrailingWhitespace) onValueNotify trailingWhitespacer.setActive

  override def keymap = Seq() // TODO
  override def configDefs = WhitespaceConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/whitespace.css") :: super.stylesheets
  override def dispose () {
    trailingWhitespacer.setActive(false)
  }
}
