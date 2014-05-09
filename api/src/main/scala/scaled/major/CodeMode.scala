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
    "C-M-\\"  -> "indent-region",
    "}"       -> "electric-close-brace"
  )

  // when editing code, we only auto-fill comments; auto-filling code is too hairy
  override def shouldAutoFill (p :Loc) = super.shouldAutoFill(p) && inComment(p)

  /** Returns true if `p` is "inside" a comment. The default implementation checks whether `p` is
    * styled with `commentFace` or `docFace`, or if `p` is the end if its line, whether the
    * character preceding `p` is thusly styled.
    */
  def inComment (p :Loc) :Boolean = {
    val ss = stylesNear(p)
    (ss contains commentStyle) || (ss contains docStyle)
  }

  /** Returns the styles for `p` unless `p` is at the end of its line, in which case the styles
    * for the character preceding `p` are returned. If the line containing `p` is empty, empty
    * styles are returned.
    */
  def stylesNear (p :Loc) :Styles = {
    val llen = buffer.lineLength(p)
    buffer.stylesAt(if (p.col < llen || llen == 0) p else p.atCol(llen-1))
  }

  // when we break for auto-fill, insert the appropriate comment prefix
  override def autoBreak (at :Loc) {
    val ss = stylesNear(at)
    super.autoBreak(at)
    (if (ss contains commentStyle) commentPrefix
     else if (ss contains docStyle) docPrefix
     else None) foreach { pre =>
       val p = view.point()
       buffer.insert(p.atCol(0), pre, Styles.None)
       view.point() = p + (0, pre.length)
       reindentAtPoint()
     }
  }

  /** The string to prepend to an auto-broken comment line. */
  def commentPrefix :Option[String] = None
  /** The string to prepend to an auto-broken doc line. */
  def docPrefix :Option[String] = None

  /** Indicates the current block, if any. Updated when bracket is inserted or the point moves. */
  val curBlock = OptValue[Block]()
  /** A helper that tracks blocks throughout the buffer. */
  val blocker = new Blocker(buffer, openBrackets, closeBrackets) {
    override def classify (row :Int, col :Int) :Int = CodeMode.this.classify(Loc(row, col))
  }

  /** Enumerates the open brackets that will be matched when tracking blocks. */
  def openBrackets = "{(["
  /** Enumerates the close brackets that will be matched when tracking blocks.
    * These must match the order of [[openBrackets]] exactly. */
  def closeBrackets = "})]"

  /** Returns an int identifying the syntax class for the character at `loc`. This is used by
    * [[blocker]] to avoid matching brackets in normal code with brackets in comments or strings.
    *
    * The default implementation treats characters styled with [[commentStyle]], [[docStyle]],
    * [[stringStyle]] and [[constantStyle]] as separate bracket classes. NOTE: zero should always
    * be returned for brackets affect actual code.
    */
  def classify (loc :Loc) :Int = {
    val styles = stylesNear(loc)
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
  def createIndenters () :List[Indenter]= Nil

  /** Computes the indentation for the line at `row`. */
  def computeIndent (row :Int) :Int = {
    // find the position of the first non-whitespace character on the line
    val line = buffer.line(row)
    val wsp = line.indexOf(isNotWhitespace)
    val start = Loc(row, if (wsp == -1) 0 else wsp)
    val block = blocker(start, 0) getOrElse Block(buffer.start, buffer.end, false)
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
    val origIndent = Indenter.readIndent(buffer, pos)
    val indent = computeIndent(pos.row)
    // shift the line, if needed
    val delta = indent - origIndent
    if (delta > 0) buffer.insert(pos.atCol(0), " " * delta, Styles.None)
    else if (delta < 0) buffer.delete(pos.atCol(0), -delta)
    // shift the point, if needed
    val p = view.point()
    if (p.row == pos.row) {
      if (p.col < origIndent) view.point() = Loc(p.row, indent)
      else view.point() = p + (0, delta)
    }
  }

  /** Reindents the line at the point. */
  def reindentAtPoint () {
    reindent(view.point())
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
