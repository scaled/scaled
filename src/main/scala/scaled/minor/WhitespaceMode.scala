//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.minor

import scala.annotation.tailrec

import scaled._
import scaled.major.{EditingMode, Syntax}

object WhitespaceConfig extends ConfigDefs {

  val showTrailingWhitespace = key(
    "If true, trailing whitespace will be highlighted", true)

  /** The CSS style applied to trailing whitespace characters. */
  val trailingWhitespaceStyle = "trailingWhitespaceFace"
}

/** A minor mode that provides whitespace manipulation fns and can highlight undesirable
  * whitespace.
  */
class WhitespaceMode (editor :Editor, config :Config, view :RBufferView, major :EditingMode)
    extends MinorMode {
  import WhitespaceConfig._

  // respond to buffer and line edits
  view.buffer.edited onValue onBufferEdit
  view.buffer.lineEdited onValue onLineEdit

  // respond to toggling of the trailing whitespace config
  config.value(showTrailingWhitespace) onValueNotify updateBufferTrailingWhitespace

  override def name = "whitespace"
  override def keymap = Seq() // TODO
  override def dispose () {
    // TODO: updateBufferTrailingWhitespace(false)?
  }

  protected def onBufferEdit (edit :Buffer.Edit) {
    if (config(showTrailingWhitespace)) {
      edit.offset until (edit.offset+edit.added) foreach updateLineTrailingWhitespace(true)
    }
  }

  protected def onLineEdit (edit :Line.Edit) {
    if (config(showTrailingWhitespace) && edit.added > 0) {
      updateLineTrailingWhitespace(true)(edit.loc.row)
    }
  }

  protected def updateBufferTrailingWhitespace (show :Boolean) {
    0 until view.buffer.lines.size foreach updateLineTrailingWhitespace(show)
  }
  protected def updateLineTrailingWhitespace (show :Boolean)(ii :Int) {
    val line = view.buffer.lines(ii)
    @tailrec def seek (col :Int) :Int = {
      if (col == 0 || major.syntax(line.charAt(col-1)) != Syntax.Whitespace) col
      else seek(col-1)
    }
    val last = line.length
    val first = seek(last)
    if (first < last) {
      // TODO: setting the style to default is presumptuous, really we want to remove the trailing
      // whitespace style if it's applied and leave it alone otherwise; I'm not sure I want to
      // support multiple faces per character, but I may have to go down that road...
      val face = if (show) trailingWhitespaceStyle else Line.defaultStyle
      view.buffer.applyStyle(face, Loc(ii, first), Loc(ii, last))
    }
  }
}
