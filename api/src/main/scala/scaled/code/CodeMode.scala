//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.major.EditingMode
import scaled.util.Chars

object CodeConfig extends Config.Defs {

  @Var("The number of characters to indent when in a nested scope.")
  val indentWidth = key(4)

  @Var("Whether to attempt to auto-detect indentWidth for buffers with existing code.")
  val autoDetectIndent = key(true)

  @Var("Enables debug logging for indentation logic.")
  val debugIndent = key(false)

  @Var("""Whether to automatically insert a close brace `}` when an open brace `{` is typed at
          the end of a line.""")
  val autoCloseBrace = key(true)

  @Var("""Whether to auto expand a block when enter is pressed between a pair of open and close
          braces (`{}`). Expansion means properly indenting the close brace on the next line and
          then inserting a blank line between the open and close brace and positioning the point
          properly indented on that line.""")
  val autoExpandBlock = key(true)

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
  /** The CSS style applied to `string` syntax. */
  val stringStyle = "codeStringFace"
  /** The CSS style applied to `module` syntax. */
  val moduleStyle = "codeModuleFace"
  /** The CSS style applied to `type` syntax. */
  val typeStyle = "codeTypeFace"
  /** The CSS style applied to `function` syntax. */
  val functionStyle = "codeFunctionFace"
  /** The CSS style applied to `variable` syntax. */
  val variableStyle = "codeVariableFace"
  /** The CSS style applied to `invalid` syntax. */
  val invalidStyle = "codeInvalidFace"

  /** The CSS style applied to the current block delimiters. */
  val blockDelimStyle = "blockDelimFace"
  /** The CSS style applied to the current block delimiters when mismatched. */
  val blockErrorStyle = "blockErrorFace"
}

/** A base class for major modes which edit program code.
  */
abstract class CodeMode (env :Env) extends EditingMode(env) {
  import CodeConfig._
  import Chars._

  override def configDefs = CodeConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("TAB",     "reindent").
    bind("ENTER",   "electric-newline").
    bind("S-ENTER", "electric-newline").
    bind("{",       "electric-open-brace").

    bind("S-C-,", "previous-bracket").
    bind("S-C-.", "next-bracket").
    bind("S-C-/", "bounce-bracket").

    bind("C-M-\\",  "indent-region").
    bind("C-c C-c", "comment-region").
    bind("C-c C-u", "uncomment-region"). // TODO: prefix args?

    bind("M-A-s", "show-syntax");

  /** A context provided to our indenters. */
  val indentCtx = new Indenter.Context {
    def buffer = CodeMode.this.buffer
    def blocker = CodeMode.this.blocker
    def debug = config(CodeConfig.debugIndent)
    def indentWidth = if (config(CodeConfig.autoDetectIndent) && detectedIndent > 0) detectedIndent
                      else config(CodeConfig.indentWidth)
    private lazy val detectedIndent = CodeMode.this.detectIndent
  }

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
    val start = Loc(row, line.firstNonWS)
    val block = blocker.require(start, Syntax.Default)
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

  /** Requests that the indent-level of the buffer be auto-detected. Modes can implement this with
    * [[Indenter.Detecter]] in combination with their own language specific understanding. `0` (the
    * default) indicates that we should use the configured indentation value (i.e. auto-detection
    * is not supported or that a reliable indentation value could not be detected).
    */
  def detectIndent :Int = 0

  // when editing code, we only auto-fill comments; auto-filling code is too hairy
  override def shouldAutoFill (p :Loc) = super.shouldAutoFill(p) && commenter.inComment(buffer, p)

  override def mkParagrapher (syn :Syntax) =
    if (syn.isComment) commenter.mkParagrapher(syn, buffer)
    else super.mkParagrapher(syn)

  // in code mode, refills only happen inside comments
  override def fillParagraph () {
    // make sure we're "looking at" a comment on this line
    val p = view.point()
    val wp = buffer.line(p).indexOf(isNotWhitespace, p.col) match {
      case -1 => p
      case ii => p.atCol(ii)
    }
    if (commenter.inComment(buffer, wp)) super.fillParagraph()
    else window.popStatus("Code modes only fill comments, not code.")
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
    view.popup() = Popup.text(info, Popup.UpRight(view.point()))
  }

  @Fn("""Recomputes the current line's indentation based on the code mode's indentation rules and
         adjusts its indentation accordingly.""")
  def reindent () {
    reindentAtPoint()
  }

  @Fn("""Inserts a newline and performs any other configured automatic actions. This includes
         indenting the cursor according to the mode's indentation rules, processing
         `auto-expand-block` (if enabled) and any other newline-triggered actions.""")
  def electricNewline () {
    val p = view.point()
    val expandBlock = config(autoExpandBlock) && (
      buffer.charAt(p.prevC) == '{') && (buffer.charAt(p) == '}')
    newline()
    if (expandBlock) {
      val bp = view.point()
      newline()
      reindentAtPoint()
      view.point() = bp
    }
    reindentAtPoint()
  }

  @Fn("""Inserts an open brace, and if the `auto-close-brace` configuration is true, inserts a close
         brace following the open brace, leaving the cursor positioned after the open brace.""")
  def electricOpenBrace () {
    selfInsertCommand("{")
    val p = view.point()
    if (p.col == buffer.line(p).length && config(autoCloseBrace)) {
      selfInsertCommand("}")
      view.point() = p
    }
  }

  @Fn("""Moves the point onto the open bracket (paren, brace, etc.) of the enclosing block. This
         places the point into the block that encloses the current block, so a repeated invocation
         of this fn will move the point to the next outermost block start, and so forth until no
         enclosing block can be found.""")
  def previousBracket () {
    curBlock.getOption match {
      case None    => window.popStatus("No enclosing block can be found.")
      case Some(b) => view.point() = b.start
    }
  }

  @Fn("""Moves the point just after the close bracket (parent, brace, etc.) of the enclosing block.
         If the point is already there, the point is moved just after the next-nearest enclosing
         block's close bracket.""")
  def nextBracket () {
    curBlock.getOption match {
      case None    => window.popStatus("No enclosing block can be found.")
      case Some(b) => if (view.point() != b.end.nextC) view.point() = b.end.nextC
                      else blocker(buffer.forward(b.end, 2)) match {
                        case None => window.popStatus("No enclosing block can be found.")
                        case Some(b) => view.point() = b.end.nextC
                      }
    }
  }

  @Fn("""Moves the point just after the open bracket (paren, brace, etc.) of the enclosing block.
         If the point is already there, the point is moved just after the block's close bracket.
         If the point is not at either end of the block when this fn is first invoked, the mark
         will be set to the current point (inside the block).

         If this fn is invoked with the point just after the block's close bracket, and the mark
         is inside the block, the point will be moved to the mark. This allows one to cycle from
         a point inside the block, to the block start, to the block end, and back to the starting
         point inside the block.""")
  def bounceBracket () {
    curBlock.getOption match {
      case None => window.popStatus("No enclosing block can be found.")
      case Some(b) =>
        val p = view.point()
        // if we're at the start of the block, move to the end of the block
        if (p == b.start.nextC) view.point() = b.end.nextC
        // if we're at the end of the block, move either to the mark (if it's set and inside the
        // block), or to the start of the block
        else if (p == b.end.nextC) view.point() = buffer.mark match {
          case Some(m) => if (b.contains(m)) m else b.start.nextC
          case None => b.start.nextC
        }
        else {
          // otherwise we're somewhere inside the block, so set the mark and move to block start
          buffer.mark = p
          view.point() = b.start.nextC
        }
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
      else window.popStatus("This code mode does not define block comment delimiters.")
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
