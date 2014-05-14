//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.code

import reactual.OptValue
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.EditingMode
import scaled.util.Chars

object CodeConfig extends Config.Defs {

  @Var("The number of characters to indent when in a nested scope.")
  val indentWidth = key(4)

  @Var("Whether to attempt to auto-detect indentWidth for buffers with existing code.")
  val autoDetectIndent = key(true)

  @Var("""When true, a } that immediately follows a { will be expanded into a properly indented
          block containing a single blank line, on which the point will be positioned.""")
  val electricBrace = key(true)

  @Var("Enables debug logging for indentation logic.")
  val debugIndent = key(false)

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
    "TAB"     -> "reindent",
    "}"       -> "electric-close-brace",

    "C-M-\\"      -> "indent-region",
    "C-c C-c"     -> "comment-region",
    "C-u C-c C-c" -> "uncomment-region", // TODO: prefix args?

    "M-A-s"       -> "show-syntax"
  )

  /** The list of indenters used to indent code for this mode. */
  val indenters :List[Indenter]

  /** A helper class for dealing with comments. */
  val commenter :Commenter

  /** Indicates the current block, if any. Updated when bracket is inserted or the point moves. */
  val curBlock = OptValue[Block]()
  /** A helper that tracks blocks throughout the buffer. */
  val blocker = new Blocker(buffer, openBrackets, closeBrackets)

  /** Enumerates the open brackets that will be matched when tracking blocks. */
  def openBrackets = "{(["
  /** Enumerates the close brackets that will be matched when tracking blocks.
    * These must match the order of [[openBrackets]] exactly. */
  def closeBrackets = "})]"

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

  /** Computes the indentation for the line at `row`. */
  def computeIndent (row :Int) :Int = {
    // find the position of the first non-whitespace character on the line
    val line = buffer.line(row)
    val wsp = line.indexOf(isNotWhitespace)
    val start = Loc(row, if (wsp == -1) 0 else wsp)
    val block = blocker(start, Syntax.Default) getOrElse Block(buffer.start, buffer.end, false)
    @tailrec @inline def loop (ins :List[Indenter]) :Int =
      if (ins.isEmpty) 0 else {
        val opt = ins.head(block, line, start)
        if (opt.isDefined) opt.get else loop(ins.tail)
      }
    loop(indenters)
  }

  /** Computes the indentation for the line at `pos` and adjusts its indentation to match. If the
    * point is in the whitespace at the start of the indented line, it will be moved to the first
    * non-whitespace character in the line, otherwise it will be shifted along with the line to
    * retain its relative position in the line.
    */
  def reindent (pos :Loc) {
    val indent = computeIndent(pos.row)
    // shift the line, if needed
    val delta = indent - Indenter.readIndent(buffer, pos)
    if (delta > 0) buffer.insert(pos.atCol(0), Line(" " * delta))
    else if (delta < 0) buffer.delete(pos.atCol(0), -delta)
    // if the point is now in the whitespace preceding the indent, move it to line-start
    val p = view.point()
    if (p.row == pos.row && p.col < indent) view.point() = Loc(p.row, indent)
  }

  /** Reindents the line at the point. */
  def reindentAtPoint () {
    reindent(view.point())
  }

  // when editing code, we only auto-fill comments; auto-filling code is too hairy
  override def shouldAutoFill (p :Loc) = super.shouldAutoFill(p) && commenter.inComment(buffer, p)

  // in code mode, paragraphs are delimited by "blank" comment lines
  override def isParagraphDelim (line :LineV) = commenter.commentStart(line) == line.length

  // in code mode, refills only happen inside comments
  override def fillParagraph () {
    // make sure we're "looking at" a comment
    val p = buffer.scanForward(isNotWhitespace, view.point())
    if (commenter.inComment(buffer, p)) super.fillParagraph()
    else editor.popStatus("Code modes only fill comments, not code.")
  }
  override def refillLinesIn (start :Loc, end :Loc) = {
    val cend = trimEnd(end)
    buffer.replace(start, cend, commenter.refilled(buffer, fillColumn, start, cend))
  }

  // when we break for auto-fill, insert the appropriate comment prefix
  override def autoBreak (at :Loc) {
    val pre = commenter.prefixFor(buffer.syntaxNear(at))
    super.autoBreak(at)
    if (pre != "") {
      buffer.insert(view.point().atCol(0), Line(pre))
      reindentAtPoint()
    }
  }

  //
  // FNs

  @Fn("Displays the syntax at the point.")
  def showSyntax () {
    val info = Seq(buffer.syntaxAt(view.point()).toString)
    view.popup() = Popup(info, Popup.UpRight(view.point()))
  }

  @Fn("Inserts a newline, then indents according to the code mode's indentation rules.")
  def newlineAndIndent () {
    newline()
    reindentAtPoint()
  }

  @Fn("""Recomputes the current line's indentation based on the code mode's indentation rules and
         adjusts its indentation accordingly.""")
  def reindent () {
    reindentAtPoint()
  }

  @Fn("""Inserts a brace, indenting it automatically to the proper position. If eletric-close-brace
         is true and the preceding character is '{', a blank line is inserted prior to the '}' and
         the point is left on the blank line, at the proper indentation.""")
  def electricCloseBrace () {
    val p = view.point()
    if (!config(electricBrace) || buffer.charAt(p.prevC) != '{') {
      selfInsertCommand("}")
      reindentAtPoint()
    } else {
      newline()
      newline()
      selfInsertCommand("}")
      reindent(view.point())
      view.point() = view.point().prevL
      reindentAtPoint()
    }
  }

  @Fn("Indents each non-blank line in the region.")
  def indentRegion () {
    withRegion { (start, end) =>
      var pos = start ; while (pos < end) {
        if (buffer.line(pos).length > 0) reindent(pos)
        pos = pos.nextL
      }
    }
  }

  @Fn("""Comments out the active region. If the start and end of the region are in column zero,
         and line comments are supported by the mode, line comments are used, otherwise the region
         is wrapped in a block comment.""")
  def commentRegion () {
    withRegion { (start, end) =>
      if (start.col == 0 && end.col == 0 && commenter.linePrefix != "") {
        buffer.replace(start, end, commenter.lineCommented(buffer, start, end))
      }
      else if (commenter.blockOpen != "") {
        commenter.blockComment(buffer, start, end)
      }
      else editor.popStatus("This code mode does not define block comment delimiters.")
    }
  }

  @Fn("""Removes comments from the active region, assuming they were uniformly applied. If the
         region starts and ends with the block comment delimiters, they will be removed. Otherwise
         the line comment delimiters will be sought and removed from the start of each line.""")
  def uncommentRegion () {
    withRegion { (start, end) =>
      if (!commenter.unBlockComment(buffer, start, end)) {
        commenter.unLineComment(buffer, start, end)
      }
    }
  }
}
