//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

object CodeConfig extends ConfigDefs {

  val indentWidth = key(
    "The number of characters to indent when in a nested scope.", 4)

  val autoDetectIndent = key(
    "Whether to attempt to auto-detect indentWidth for buffers with existing code.", true)

  /** The CSS style applied to `builtin` syntax. */
  val builtinStyle = "codeBuiltinFace"
  /** The CSS style applied to `preprocessor` syntax. */
  val preprocessorStyle = "codePreprocessorFace"
  /** The CSS style applied to `comment` syntax. */
  val commentStyle = "codeCommentFace"
  /** The CSS style applied to `constant` syntax. */
  val constantStyle = "codeConstantFace"
  /** The CSS style applied to `doc` syntax. */
  val docStyle = "codeDocFace"
  /** The CSS style applied to `keyword` syntax. */
  val keywordStyle = "codeKeywordFace"
  /** The CSS style applied to `reference` syntax. */
  val referenceStyle = "codeReferenceFace"
  /** The CSS style applied to `string` syntax. */
  val stringStyle = "codeStringFace"
  /** The CSS style applied to `type` syntax. */
  val typeStyle = "codeTypeFace"
  /** The CSS style applied to `variable` syntax. */
  val variableStyle = "codeVariableFace"
  /** The CSS style applied to `function` syntax. */
  val functionStyle = "codeFunctionFace"
}

/** A base class for major modes which edit program code.
  */
abstract class CodeMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends EditingMode(editor, config, view, disp) {
  import CodeConfig._

  override def configDefs = CodeConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
  override def keymap = super.keymap ++ Seq(
    "ENTER" -> "newline-and-indent",
    "TAB"   -> "reindent"
  )

  /** Returns the number of whitespace chars at the start of `line`. */
  def readIndent (line :LineV) :Int = {
    var pos = 0 ; val end = line.length
    while (pos < end && syntax(line.charAt(pos)) == Syntax.Whitespace) pos += 1
    pos
  }

  /** Computes the indentation for the line at `pos`. */
  def computeIndent (pos :Loc) :Int = if (pos.row == 0) 0 else {
    def findNonBlank (row :Int) :LineV = {
      val line = buffer.line(row)
      if (row == 0 || line.length > 0) line
      else findNonBlank(row-1)
    }
    val prev = findNonBlank(pos.row-1)
    val prevIndent = readIndent(prev)
    // if the previous line introduced a new scope, indent from that
    if (opensScope(prev)) prevIndent + config(indentWidth)
    // if the previous line opened an arg list, indent from that
    else opensArgList(prev) match {
      case -1 => prevIndent // otherwise indent the same as the previous line
      case ii => ii
    }
  }

  /** Returns true if `line` opens a new scope.
    * By default this means it has `{` without a closing `}`. */
  def opensScope (line :LineV) :Boolean = line.lastIndexOf('{') > line.lastIndexOf('}')

  /** Returns arg list indent column if `line` opens an argument list, -1 otherwise.
    * By default this looks for `(` without a closing `)` and returns the column immediately to the
    * right of the `(`. */
  def opensArgList (line :LineV) :Int = {
    val oidx = line.lastIndexOf('(')
    if (oidx > line.lastIndexOf(')')) oidx+1 else -1
  }

  /** Computes the indentation for the line at `pos` and adjusts its indentation to match. */
  def reindent (pos :Loc) :Loc = {
    val indent = computeIndent(pos)
    val curIndent = readIndent(buffer.line(pos))
    val delta = indent - curIndent
    if (delta > 0) buffer.insert(pos.atCol(0), " " * delta, Styles.None)
    else if (delta < 0) buffer.delete(pos.atCol(0), -delta)

    // if the point is in the whitespace at the start of the line, move it to the start of the
    // line; otherwise shift it by the amount of whitespace adjusted
    val p = view.point()
    view.point() = if (p.row != pos.row) p
                   else if (p.col < indent) p.atCol(indent)
                   else p + (0, delta)
  }

  @Fn("Inserts a newline, then indents according to the code mode's indentation rules.")
  def newlineAndIndent () {
    newline()
    reindent(view.point())
  }

  @Fn("""Recomputes the current line's indentation based on the code mode's indentation rules and
         adjusts its indentation accordingly.""")
  def reindent () {
    reindent(view.point())
  }
}
