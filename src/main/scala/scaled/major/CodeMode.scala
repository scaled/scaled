//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.OptValue
import scala.annotation.tailrec
import scaled._
import scaled.util.{Block, Blocker, Chars, Indenter}

object CodeConfig extends Config.Defs {

  @Var("The number of characters to indent when in a nested scope.")
  val indentWidth = key(4)

  @Var("Whether to attempt to auto-detect indentWidth for buffers with existing code.")
  val autoDetectIndent = key(true)

  @Var("""When true, a } that immediately follows a { will be expanded into a properly indented
          block containing a single blank line, on which the point will be positioned.""")
  val electricBrace = key(true)

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
    "C-M-\\"  -> "indent-region",
    "}"       -> "electric-close-brace"
  )

  /** Enumerates the open brackets that will be matched when tracking blocks. */
  protected def openBrackets = "{(["
  /** Enumerates the close brackets that will be matched when tracking blocks.
    * These must match the order of [[openBrackets]] exactly. */
  protected def closeBrackets = "})]"

  /** Indicates the current block, if any. Updated when bracket is inserted or the point moves. */
  val curBlock = OptValue[Block]()
  /** A helper that tracks blocks throughout the buffer. */
  val blocker = new Blocker(buffer, openBrackets, closeBrackets) {
    override def classify (row :Int, col :Int) :Int = CodeMode.this.classify(Loc(row, col))
  }

  /** Returns an int identifying the syntax class for the character at `loc`. This is used by
    * [[blocker]] to avoid matching brackets in normal code with brackets in comments or strings.
    *
    * The default implementation treats characters styled with [[commentStyle]], [[docStyle]],
    * [[stringStyle]] and [[constantStyle]] as separate bracket classes. NOTE: zero should always
    * be returned for brackets affect actual code.
    */
  protected def classify (loc :Loc) :Int = {
    val styles = buffer.stylesAt(loc)
    if (styles.contains(commentStyle)) 1
    else if (styles.contains(docStyle)) 2
    else if (styles.contains(stringStyle)) 3
    else if (styles.contains(constantStyle)) 4
    else 0
  }

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

  /** The list of indenters used to indent code for this mode. */
  val indenters :List[Indenter] = createIndenters()
  protected def createIndenters () :List[Indenter]= List(Indenter.ByBlock)

  /** Computes the indentation for the line at `row`. */
  def computeIndent (row :Int) :Int = {
    // find the position of the first non-whitespace character on the line
    val line = buffer.line(row)
    val wsp = line.find(isNotWhitespace)
    val start = Loc(row, if (wsp == -1) 0 else wsp)
    val block = blocker(start, 0) getOrElse Block(buffer.start, buffer.end, false)
    @tailrec @inline def loop (ins :List[Indenter]) :Int =
      if (ins.isEmpty) 0 else {
        val opt = ins.head(config, buffer, block, line, start)
        if (opt.isDefined) opt.get else loop(ins.tail)
      }
    loop(indenters)
  }

  /** Computes the indentation for the line at `pos` and adjusts its indentation to match. If the
    * point is on the line at `pos` it will be adjusted to account for the changed indentation.
    * @return the new indentation for the line at `pos`.
    */
  def reindent (pos :Loc) :Int = {
    val origIndent = Indenter.readIndent(buffer, pos)
    val indent = computeIndent(pos.row)
    val delta = indent - origIndent
    if (delta > 0) buffer.insert(pos.atCol(0), " " * delta, Styles.None)
    else if (delta < 0) buffer.delete(pos.atCol(0), -delta)
    // shift the point if appropriate
    if (delta != 0) {
      val p = view.point()
      if (p.row == pos.row && p.col >= origIndent) view.point() = p + (0, delta)
    }
    indent
  }

  /** Reindents the line at the point. If the point is in the whitespace at the start of the line,
    * it will be moved to the first non-whitespace character in the line. */
  def reindentAtPoint () {
    val indent = reindent(view.point())
    // if the point is in the whitespace at the start of the line, move it to the start
    val p = view.point()
    if (p.col < indent) view.point() = p.atCol(indent)
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
}
