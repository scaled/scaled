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

  @Var("Enables debug logging for indentation logic.")
  val debugIndent = key(false)

  @Var("""Whether to automatically insert a close bracket when an open bracket is typed at
          the end of a line. See `electric-open-bracket` for details.""")
  val autoCloseBracket = key(true)

  @Var("""When enabled, inserting a (single or double) quote will insert a matching close quote
          and position the point between the quotes. See `electric-quote` for more details.""")
  val autoCloseQuote = key(true)

  @Var("""Whether to auto expand a block when enter is pressed between a pair of open and close
          brackets (`{}`). Expansion means properly indenting the close bracket on the next line
          and then inserting a blank line between the open and close bracket and positioning the
          point properly indented on that line.""")
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

/** A base class for major modes which edit program code. */
abstract class CodeMode (env :Env) extends EditingMode(env) {
  import CodeConfig._
  import Chars._

  override def configDefs = CodeConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("reindent",               "TAB").
    bind("electric-newline",       "ENTER", "S-ENTER").
    bind("electric-open-bracket",  "{", "(", "[").
    bind("electric-close-bracket", "}", ")", "]").
    bind("electric-quote",         "\"", "'").

    bind("previous-bracket", "S-C-,").
    bind("next-bracket",     "S-C-.").
    bind("bounce-bracket",   "S-C-/").

    bind("indent-region",    "C-M-\\").
    bind("comment-region",   "C-c C-c").
    bind("uncomment-region", "C-c C-u"). // TODO: prefix args?

    bind("show-syntax", "M-A-s");

  /** Handles indentation for this buffer. */
  lazy val indenter :Indenter = createIndenter()
  protected def createIndenter () = new Indenter(config)

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
  def computeIndent (row :Int) :Int = indenter(row)

  /** Computes the indentation for the line at `pos` and adjusts its indentation to match. If the
    * point is in the whitespace at the start of the indented line, it will be moved to the first
    * non-whitespace character in the line, otherwise it will be shifted along with the line to
    * retain its relative position in the line.
    */
  def reindent (pos :Loc) {
    val indent = computeIndent(pos.row)
    if (indent >= 0) {
      // shift the line, if needed
      val delta = indent - Indenter.readIndent(buffer, pos)
      if (delta > 0) buffer.insert(pos.atCol(0), Line(" " * delta))
      else if (delta < 0) buffer.delete(pos.atCol(0), -delta)
      // if the point is now in the whitespace preceding the indent, move it to line-start
      val p = view.point()
      if (p.row == pos.row && p.col < indent) view.point() = Loc(p.row, indent)
    } else window.emitStatus(s"Indenter has lost the plot [indent=$indent]. Ignoring.")
  }

  /** Reindents the line at the point. */
  def reindentAtPoint () :Unit = reindent(view.point())

  /** Returns the close bracket for the supplied open bracket.
    * `?` is returned if the open bracket is unknown. */
  def closeForOpen (bracket :Char) = bracket match {
    case '{' => '}'
    case '(' => ')'
    case '[' => ']'
    case '"' => '"'
    case '\'' => '\''
    case _   => '?'
  }

  /** Used to tag auto-inserted close quote/brackets. */
  case class AutoPair (openLoc :Loc)

  /** Inserts `open` and the corresponding close quote/bracket, leaving the point between the two.
    * The close character will be tagged as auto-inserted so that the electric-foo methods know
    * that it may be overwritten if the user manually types the close character over top of it. */
  def insertTaggedPair (open :Char) {
    val fst = view.point()
    val mid = buffer.insert(fst, open, Syntax.Default)
    val end = buffer.insert(mid, closeForOpen(open), Syntax.Default)
    buffer.addTag(AutoPair(fst), mid, end)
    view.point() = mid
  }

  /** Returns true if `loc` contains an auto-inserted close quote/bracket which matches `close`
    * and for which the auto-closed open quote/bracket appears to still be valid. */
  def isTaggedClose (loc :Loc, close :Char) :Boolean = {
    buffer.charAt(loc) == close && (buffer.tagAt(classOf[AutoPair], loc) match {
      case None     => false
      case Some(ap) => closeForOpen(buffer.charAt(ap.openLoc)) == close
    })
  }

  override def shouldAutoFill (p :Loc) = super.shouldAutoFill(p) && canAutoFill(p)
  // when editing code, we only auto-fill comments; auto-filling code is too hairy
  protected def canAutoFill (p :Loc) :Boolean = commenter.inComment(buffer, p)

  override def mkParagrapher (syn :Syntax) =
    if (syn.isComment) commenter.mkParagrapher(syn, buffer)
    else super.mkParagrapher(syn)

  override def fillParagraph () {
    // make sure we're "looking at" something fillable on this line
    val p = view.point()
    val wp = buffer.line(p).indexOf(isNotWhitespace, p.col) match {
      case -1 => p
      case ii => p.atCol(ii)
    }
    if (canAutoFill(wp)) super.fillParagraph()
    else window.popStatus(s"$name-mode doesn't know how to fill this paragraph.")
  }

  override def refillLinesIn (start :Loc, end :Loc) =
    if (!commenter.inComment(buffer, view.point())) super.refillLinesIn(start, end)
    else {
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

  @Fn("""Inserts an open bracket, and if the `auto-close-bracket` configuration is true, inserts
         a close bracket following the open bracket, leaving the cursor positioned after the open
         bracket.""")
  def electricOpenBracket (typed :String) {
    val p = view.point()
    // TODO: instead just scan ahead a bit for the expected close and don't pair if we see it...
    if (config(autoCloseBracket) && p == buffer.lineEnd(p)) insertTaggedPair(typed.charAt(0))
    // if auto-close bracket is disabled or we're not at the end of the line, no magic!
    else selfInsertCommand(typed)
  }

  @Fn("""Automatically indents a close-bracket immediately after typing it. Also avoids duplicating
         the close bracket if the needed bracket is already under the point.""")
  def electricCloseBracket (typed :String) {
    val p = view.point()
    // if we're currently on the desired close bracket, just pretend like we typed it and fwd-char
    if (isTaggedClose(p, typed.charAt(0))) view.point() = p.nextC
    else {
      selfInsertCommand(typed)
      reindentAtPoint()
    }
  }

  @Fn("""Inserts a (single or double) quote and, if the point is currently on whitespace and in
         code syntax (not string or comment), a matching close quote. Leaves the point between
         the quotes.
         If the point is currently on the to-be-inserted quote, nothing is inserted and the point
         is simply moved past the quote. This avoids undesirable behavior if the user's fingers
         insist on typing the close quote even though it was already inserted for them.""")
  def electricQuote (typed :String) :Unit =
    if (!config(autoCloseQuote)) selfInsertCommand(typed)
    else {
      val p = view.point()
      // if we're currently on the desired quote, just pretend like we typed it and move forward
      if (isTaggedClose(p, typed.charAt(0))) view.point() = p.nextC
      // if we're not looking at whitespace or not in code mode, no magic
      else if (!isWhitespace(buffer.charAt(p)) ||
               !buffer.syntaxNear(p).isCode) selfInsertCommand(typed)
      // otherwise auto-pair the quote and leave the point betwixt
      else insertTaggedPair(typed.charAt(0))
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
