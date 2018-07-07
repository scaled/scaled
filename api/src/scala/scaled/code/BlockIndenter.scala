//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import scaled._

/** An indenter that indents lines based on blocks and expressions. */
class BlockIndenter (cfg :Config, rules :SeqV[BlockIndenter.Rule]) extends Indenter(cfg) {
  import Indenter._
  import BlockIndenter._

  override def apply (info :Info) :Int = {
    state(info, computeState) match {
      // if this character closes the block on the top of the stack, handle it specially because
      // the close bracket is not indented as if it were inside the block
      case bs :BlockS if (bs.isClose(info.firstChar)) =>
        // by default we just indent based on the state above block state on the stack
        activeRules.foldLeft(bs.next)((s, r) => r.adjustCloseIndent(s)).indent(config, false)
      case st =>
        val base = st.indent(config, true)
        activeRules.foldLeft(base)((i, r) => r.adjustIndent(st, info, indentWidth, i))
    }
  }

  protected def activeRules = rules.filter(_.active(config))

  protected def computeState (line :LineV, start :State) :State = {
    val openChars = this.openChars ; val closeChars = this.closeChars
    val first = line.firstNonWS ; val last = lastNonWS(line)
    var state = activeRules.foldLeft(start)((s, r) => r.adjustStart(line, first, last, s))

    // scan the line one character at a time, looking for open/close braces and parens
    var cc = 0 ; val ll = line.length ; while (cc < ll) {
      val c = line.charAt(cc) ; val s = line.syntaxAt(cc)
      if (s.isCode) {
        val idx = openChars.indexOf(c)
        if (idx >= 0) state = openBlock(line, c, closeChars.charAt(idx), cc, state)
        else if (closeChars.indexOf(c) >= 0) state = closeBlock(line, c, cc, state)
      }
      cc += 1
    }

    activeRules.foldLeft(state)((s, r) => r.adjustEnd(line, first, last, start, s))
  }

  protected def openBlock (line :LineV, open :Char, close :Char, col :Int, state :State) :State = {
    val top = activeRules.foldLeft(state)((s, r) => r.willOpenBlock(line, open, close, col, s))
    // if the open char is the last non-comment character of the line, then we treat it like a
    // block rather than an expression regardless of whether it's a curly brace, paren, etc.
    val last = lastNonWS(line)
    if (col >= last) new BlockS(close, -1, top)
    else if (isExprOpen(open)) new ExprS(close, col, top)
    else new BlockS(close, col, top)
  }
  protected def closeBlock (line :LineV, close :Char, col :Int, state :State) :State = {
    activeRules.foreach(_.willCloseBlock(line, close, col, state))
    activeRules.foldLeft(state.popBlock(close))((s, r) => r.didCloseBlock(line, close, col, s))
  }

  protected def openChars = "{(["
  protected def closeChars = "})]"
  protected def isExprOpen (c :Char) = (c != '{')
}

object BlockIndenter {
  import Indenter._

  /** Indicates that we're nested in a bracketed block of code. */
  class BlockS (close :Char, col :Int, next :State) extends State(next) {
    def isClose (close :Char) :Boolean = this.close == close
    /** Returns a copy of this block with EOL closing column. */
    def makeEOL = new BlockS(close, -1, next)
    // a block bracket only affects indentation if it's at the end of a line;
    // if we see {{, for example, we only want the last one to affect indentation
    override def indent (cfg :Config, top :Boolean) :Int =
      if (col >= 0) next.indent(cfg) else super.indent(cfg, top)
    override def popBlock (close :Char) = if (this.close == close) next else next.popBlock(close)
    override protected def show :String = s"BlockS($close, $col)"
  }

  /** Indicates that we're nested in a bracketed expression. */
  class ExprS (close :Char, val col :Int, next :State) extends State(next) {
    def isClose (close :Char) :Boolean = this.close == close
    override def indent (cfg :Config, top :Boolean) :Int =
      // an expr block only affects indentation if it's on the top of the stack
      if (top) col+1 else next.indent(cfg)
    override def popBlock (close :Char) = if (this.close == close) next else next.popBlock(close)
    override protected def show :String = s"ExprS($close, $col)"
  }

  /** Defines a rule for the `BlockIndenter`. */
  abstract class Rule {
    /** Checks whether this rule is active. */
    def active (config :Config) :Boolean = true
    /** Adjusts the indent computed for a line. */
    def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) :Int = base
    /** Adjusts the state used to compute the indent after a block close. */
    def adjustCloseIndent (closeState :State) :State = closeState
    /** Adjusts the start state computed for a line. */
    def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = start
    /** Adjusts the end state computed for a line. */
    def adjustEnd (line :LineV, first :Int, last :Int, start :State, end :State) :State = end
    /** Adjusts the top state immediately prior to opening a block. */
    def willOpenBlock (line :LineV, open :Char, close :Char, col :Int, state :State) :State = state
    /** Called when a block is about to be closed. Allows rule to update internal state. */
    def willCloseBlock (line :LineV, close :Char, col :Int, state :State) :Unit = {}
    /** Called when a block has been closed. */
    def didCloseBlock (line :LineV, close :Char, col :Int, state :State) :State = state
  }

  /** Adjusts indent by multiple of base indent width (`levels`) when `pred` matches. */
  def adjustIndentWhen (pred :(State, Info) => Boolean, levels :Int) = new Rule {
    override def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) =
      if (pred(state, info)) base + levels * indentWidth else base
  }
  /** Adjusts indent by multiple of base indent width (`levels`) when `patM` matches start of
    * to-be-indented line. */
  def adjustIndentWhenMatchStart (patM :Matcher, levels :Int) :Rule =
    adjustIndentWhen((state, info) => info.startsWith(patM), levels)

  /** An indenter rule that handles indentation of continued statements. */
  class ContinuedStatementRule (contChars :String) extends Rule {
    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, cur :State) =
      // if this line is blank or contains only comments; do not mess with our "is continued or
      // not" state; wait until we get to a line with something actually on it
      if (line.synIndexOf(s => !s.isComment, first) == -1) cur
      else {
        // determine (heuristically) whether this line appears to be a complete statement
        val isContinued = (last >= 0) && this.isContinued(line, first, last, line.charAt(last))
        val isComplete = this.isComplete(isContinued, cur)
        // if we appear to be a complete statement, pop any continued statement state off the stack
        if (isComplete) adjustCompleteEnd(line, cur)
        // if we're not already a continued statement, we may need to start being so
        else if (isContinued) new ContinuedS(cur.popIf(_.isInstanceOf[ContinuedS]))
        // otherwise stick with what we have
        else cur
      }

    protected def isContinued (line :LineV, first :Int, last :Int, lastC :Char) :Boolean =
      contChars.indexOf(lastC) >= 0
    protected def isComplete (isContinued :Boolean, cur :State) =
      !(isContinued || cur.isInstanceOf[BlockS] || cur.isInstanceOf[ExprS])
    protected def adjustCompleteEnd (line :LineV, end :State) :State =
      end.popIf(_.isInstanceOf[ContinuedS])
  }

  /** Handles cont. statements with special cause for `case` statements in C-like languages. */
  class CLikeContStmtRule (contChars :String) extends ContinuedStatementRule(contChars) {
    def this () = this(".+-=?:")
    override def isContinued (line :LineV, first :Int, last :Int, lastC :Char) =
      if (!super.isContinued(line, first, last, lastC)) false
      else if (lastC != ':') true
      else !line.matches(caseColonM, first)
  }

  class ContinuedS (next :State) extends State(next) {
    override def indent (config :Config, top :Boolean) = next match {
      // if we're a continued statement directly inside an expr block, let the expr block dictate
      // alignment rather than our standard extra indents
      case nx :ExprS => nx.indent(config, top)
      case _         => super.indent(config, top)
    }
    override def show = "ContinuedS"
  }

  /** Special handling for javadoc-style block comments. */
  class BlockCommentRule (firstLineDocM :Matcher) extends Rule {
    def this () = this(Matcher.regexp("""/\*\*\s*\S+"""))

    override def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = {
      if (countComments(line, first) <= 0) start
      // this line opens a block or doc comment, so push a state for it
      else {
        // if this is a doc comment which is followed by non-whitespace,
        // then indent to match the second star rather than the first
        val col = if (line.matches(firstLineDocM, first)) 2 else 1
        new CommentS(col, start)
      }
    }

    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, end :State) :State = {
      // if this line closes a doc/block comment, pop our comment state from the stack
      if (countComments(line, 0) < 0) end.popIf(_.isInstanceOf[CommentS]) else end
    }
  }

  class CommentS (inset :Int, next :State) extends State(next) {
    override def indent (config :Config, top :Boolean) = inset + next.indent(config)
    override def show = s"CommentS($inset)"
  }

  /** If a line starts with a `.`, aligns it to the first `.` on the previous line.
    * Handles a common OO method call chaining style. */
  class AlignUnderDotRule extends Rule {
    override def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) =
      if (info.row == 0 || info.firstChar != '.') base
      else info.buffer.line(info.row-1).indexOf('.') match {
        case -1 => base
        case pp => pp
      }
  }

  /** Handles indentation of `case` blocks inside `switch` statements (in C-like languages). */
  class SwitchRule extends Rule {
    // pop case statements out one indentation level (undoing the extra indent for everything
    // inside a switch block, just for the case statement)
    override def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) =
      if (info.startsWith(caseColonM)) base - indentWidth else base
    override def adjustCloseIndent (closeState :State) =
      // if the top of the stack after a block is a switch, then skip it
      if (closeState.isInstanceOf[SwitchS]) closeState.next else closeState
    override def adjustStart (line :LineV, first :Int, last :Int, start :State) =
      if (indentCaseBlocks && line.matches(switchM, first)) new SwitchS(start) else start
    override def didCloseBlock (line :LineV, close :Char, col :Int, state :State) =
      // if there's a SwitchS on top of the stack after we pop a } block, pop it off too
      if (close == '}' && state.isInstanceOf[SwitchS]) state.next else state
    protected def indentCaseBlocks = true
    private val switchM = Matcher.regexp("switch\\b")
  }

  class SwitchS (next :State) extends State(next) {
    override def show = "SwitchS"
  }

  class LambdaBlockRule (arrow :String) extends Rule {
    // if the top of the stack is a BlockS but the end of the line is => then we're in a lambda
    // and need to adjust the BlockS to let it know that it actually should trigger indent
    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, cur :State) :State = {
      var end = cur
      if (end.isInstanceOf[BlockS]) {
        val arrowStart = last+1-lambdaArrowM.show.length
        if (arrowStart >= 0 && line.matches(lambdaArrowM, arrowStart)) {
          end = end.asInstanceOf[BlockS].makeEOL
        }
      }
      end
    }
    private val lambdaArrowM = Matcher.exact(arrow)
  }

  private val caseColonM = Matcher.regexp("(case\\s|default).*:")
}
