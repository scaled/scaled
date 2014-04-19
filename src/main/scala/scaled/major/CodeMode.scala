//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.OptValue
import scaled._
import scaled.util.{Block, Blocker, Chars}

object CodeConfig extends Config.Defs {

  @Var("The number of characters to indent when in a nested scope.")
  val indentWidth = key(4)

  @Var("Whether to attempt to auto-detect indentWidth for buffers with existing code.")
  val autoDetectIndent = key(true)

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

  /** The CSS style applied to the current block delimiters. */
  val blockDelimStyle = "codeBlockDelimFace"
  /** The CSS style applied to the current block delimiters when mismatched. */
  val blockErrorStyle = "codeBlockErrorFace"
}

/** A base class for major modes which edit program code.
  */
abstract class CodeMode (env :Env) extends EditingMode(env) {
  import CodeConfig._
  import Chars._

  override def configDefs = CodeConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
  override def keymap = super.keymap ++ Seq(
    "ENTER"   -> "newline-and-indent",
    "S-ENTER" -> "newline-and-indent",
    "TAB"     -> "reindent"
  )

  /** Enumerates the open brackets that will be matched when tracking blocks. */
  protected def openBrackets = "{(["
  /** Enumerates the close brackets that will be matched when tracking blocks.
    * These must match the order of [[openBrackets]] exactly. */
  protected def closeBrackets = "})]"

  /** Indicates the current block, if any. Updated when bracket is inserted or the point moves. */
  val curBlock = OptValue[Block]()
  /** A helper that tracks blocks throughout the buffer. */
  val blocker = new Blocker(buffer, openBrackets, closeBrackets)
  // as the point moves around, track the active block
  view.point onValue { p => curBlock() = blocker(p) }
  // as the active block changes, highlight the delimiters
  curBlock onChange { (nb, ob) =>
    ob.map { b =>
      buffer.removeStyle(blockDelimStyle, b.start, b.start.nextC)
      if (b.isValid) buffer.removeStyle(blockDelimStyle, b.end, b.end.nextC)
      else buffer.removeStyle(blockErrorStyle, b.start, b.start.nextC)
    }
    nb.map { b =>
      buffer.addStyle(blockDelimStyle, b.start, b.start.nextC)
      if (b.isValid) buffer.addStyle(blockDelimStyle, b.end, b.end.nextC)
      else buffer.addStyle(blockErrorStyle, b.start, b.start.nextC)
    }
  }

  /** Returns the number of whitespace chars at the start of `line`. */
  def readIndent (line :LineV) :Int = {
    var pos = 0 ; val end = line.length
    while (pos < end && isWhitespace(line.charAt(pos))) pos += 1
    pos
  }

  /** Computes the indentation for the line at `row`. */
  def computeIndent (row :Int) :Int = {
    // find the position of the first non-whitespace character on the line
    val wsp = buffer.line(row).find(isNotWhitespace)
    val start = Loc(row, if (wsp == -1) 0 else wsp)
    blocker(start) match {
      // locate the innermost block that contains start
      case Some(b) =>
        buffer.charAt(b.start) match {
          // if the block is an arglist (or brackets, TODO?), indent to just after the '('
          case '(' | '[' => b.start.col + 1
          // use block indentation for { and anything we don't grok
          case _ =>
            val bstart = readIndent(buffer.line(b.start))
            // if the first non-whitespace character is our close brace, use the same indent
            // as the line with the open brace
            if (b.isValid && b.end == start) bstart
            // otherwise indent one from there
            else bstart + config(indentWidth)
        }

      case None => 0 // TODO: is this always proper?
    }
  }

  /** Computes the indentation for the line at `pos` and adjusts its indentation to match. */
  def reindent (pos :Loc) :Loc = {
    val indent = computeIndent(pos.row)
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

  override def selfInsertCommand (typed :String) {
    super.selfInsertCommand(typed)
    // TODO: this seems kind of hacky;
    // we should at least have a postInsertHook() in EditingMode or something
    if (typed == "}") reindent(view.point())
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
