//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import java.util.function.Predicate
import scaled._
import scaled.util.Chars

/** Encapsulates a strategy for indenting a line of code. */
class Indenter (val config :Config) {
  import Indenter._

  /** Computes the indentation for the `row`th line of the buffer. */
  def apply (buffer :Buffer, row :Int) :Int = {
    val line = buffer.line(row) ; val first = line.firstNonWS
    // we compute the indent even if we're going to ignore it,
    // so that the proper line states get applied
    val indent = apply(Info(buffer, row, line, line.firstNonWS))
    if (NoIndentSyntaxes(line.syntaxAt(first))) first else indent
  }

  /** Computes the indentation for the line identified by `info`. */
  def apply (info :Info)  = 0

  /** Returns an indentation `steps` steps inset from `base`.
    * @param base the default indentation of the line (in characters, not steps). */
  protected def indentFrom (base :Int, steps :Int) :Int = base + steps * indentWidth

  /** Returns the configured indent width. */
  protected def indentWidth = config(CodeConfig.indentWidth)

  /** Issues an indentation debugging message. */
  protected def debug (msg :String) :Unit = if (config(CodeConfig.debugIndent)) println(msg)

  /** When the first character of a line is one of this set of syntaxes, we preserve indentation as
    * is instead of computing a new indentation and applying it. */
  protected val NoIndentSyntaxes :Set[Syntax] = Set(Syntax.HereDocLiteral)
}

object Indenter {
  import Chars._

  /** Encapsulates info passed to an indenter to indent a single line. */
  case class Info (buffer :Buffer, row :Int, line :LineV, first :Int) {
    /** Returns the first non-WS character on our line. */
    def firstChar = line.charAt(first)
    /** Returns true if the first non-WS characters on our line match `matcher`. */
    def startsWith (matcher :Matcher) = line.matches(matcher, first)
  }

  /** Returns the number of whitespace chars at the start of `line`. */
  def readIndent (line :LineV) :Int =
    // TODO: this will eventually have to handle tabs; we'll probably just scan the line oursevles
    // ticking up our counter 1 for each space and tab-width for each tab
    line.firstNonWS

  /** Returns the number of whitespace chars at the start of the line at `pos`. */
  def readIndent (buffer :BufferV, pos :Loc) :Int = readIndent(buffer.line(pos))

  /** Returns true if `m` matches the first non-whitespace characters of `line`. */
  def startsWith (line :LineV, m :Matcher) :Boolean = line.matches(m, line.firstNonWS)

  /** Returns true if `m` matches the last non-whitespace characters of `line`. */
  def endsWith (line :LineV, m :Matcher) :Boolean = line.lastIndexOf(m) match {
    case -1 => false
    case ii => line.indexOf(isNotWhitespace, ii+m.matchLength) == -1
  }

  /** Returns the column of the last non-whitespace-or-comment character of `line`,
    * or -1 if it contains no such character. */
  def lastNonWS (line :LineV) :Int = {
    def loop (from :Int) :Int = line.lastIndexOf(Chars.isNotWhitespace, from) match {
      case -1 => -1
      case ii => if (!line.syntaxAt(ii).isComment) ii else loop(ii-1)
    }
    loop(line.length-1)
  }

  /** Scans `line` looking for open (slash star) and close (star slash) comments and the number of
    * opens minus the number of closes.
    * @param from the character offset in `line` at which to start counting. */
  def countComments (line :LineV, from :Int = 0) :Int = {
    var res = 0
    var ii = from ; val ll = line.length ; while (ii < ll) {
      val c = line.charAt(ii) ; val n = line.charAt(ii+1)
      val ps = if (ii == 0) Syntax.Default else line.syntaxAt(ii-1)
      if (c == '/' && n == '*' && ps.isCode) res += 1
      else if (c == '*' && n == '/' && line.syntaxAt(ii+2).isCode) res -= 1
      ii += 1
    }
    res
  }

  /** Tracks buffer elements that change the indentation state. For the `BlockIndenter` these
    * include things like `BlockS` (a block of code, often nested in curly braces), `ExprS` (an
    * expression that spans multiplie lines), `ContinuedS` (a continued statement), etc..
    *
    * A stack of elements describes the indentation state at the start of a given line. The state
    * is combined with information local to that line to determine the indentation of the line as
    * well as the indentation state at the end of that line.
    */
  abstract class State (val next :State) extends Line.Tag {
    /** Computes the base indentation for this state.
      * @param top whether this state is on the top of the stack. */
    def indent (cfg :Config, top :Boolean = false) :Int = indentWidth(cfg) + next.indent(cfg)

    /** Pops this state off the stack if `f` returns true. */
    def popIf (f :Predicate[State]) :State = if (f.test(this)) next else this

    /** Pops state off the stack until it pops off a state for which `f` returns true. */
    def popTo (f :State => Boolean) :State = if (f(this)) next else next.popTo(f)
    /** Pops state off the stack until it pops off a state for which `f` returns true. */
    def popTo (f :Predicate[State]) :State = if (f.test(this)) next else next.popTo(f)

    /** Pops state off the stack until it pops off a block, or reaches the empty state. */
    def popBlock (close :Char) :State = next.popBlock(close)

    override def key = classOf[State]
    override def ephemeral :Boolean = true

    override def toString = {
      val sb = new StringBuilder()
      var ss = this ; while (ss != EmptyS) {
        if (sb.length > 0) sb.append(" ")
        sb.append(ss.show)
        ss = ss.next
      }
      sb.toString
    }

    protected def indentWidth (config :Config) :Int = config(CodeConfig.indentWidth)
    protected def show :String
  }

  /** The state at the start of a buffer: nothing. */
  val EmptyS :State = new State(null) {
    override def indent (cfg :Config, top :Boolean) = 0
    override def popTo (f :State => Boolean) = this
    override def popBlock (close :Char) = this
    override def toString = show
    override protected def show :String = "EmptyS"
  }

  /** A marker state indicating that a line's state needs to be computed. */
  val UnknownS :State = new State(null) {
    override def indent (cfg :Config, top :Boolean) = ???
    override def popTo (f :State => Boolean) = ???
    override def popBlock (close :Char) = ???
    override def toString = show
    override protected def show :String = "UnknownS"
  }

  /** An indenter that uses per-line state to compute indentation. */
  abstract class ByState (cfg :Config) extends Indenter(cfg) {

    override def apply (info :Info) :Int = {
      val lstate = state(info, computeState)
      computeIndent(lstate, lstate.indent(config, true), info)
    }

    /** Computes the state at the end of `line` given the starting state `start`. */
    protected def computeState (line :LineV, start :State) :State

    /** Computes the indent for `line`. The default implementation simply returns `base`.
      * @parma state the indentation state prior to processing `line`.
      * @param base the base indent for `line` based on braces, parens, etc.
      * @param info the info on the line whose indentation is being computed.
      * @return the column to which `line` should be indented.
      */
    protected def computeIndent (state :State, base :Int, info :Info) :Int = base
  }

  /** Computes the state for a line given the supplied `stater` function. Any unstated lines prior
    * to the target line will also have their state computed and stored in line state as
    * indentation state is computed incrementally from the start of the buffer, reusing the state
    * from the previous line for each successive line. */
  def state (info :Info, stater :(LineV, State) => State) :State = {
    // scan backwards until we reach the start of the buffer or a line with known state
    val buffer = info.buffer
    var ss = 0 ; var sstate = EmptyS ; var ll = info.row-1 ; while (ll > 0) {
      val lstate = buffer.line(ll).lineTag(UnknownS)
      if (lstate == UnknownS) ll -= 1
      else { ss = ll ; sstate = lstate ; ll = 0 /*break*/ }
    }

    // now proceed forward, computing and storing line state
    buffer.setLineTag(ss, sstate)
    while (ss < info.row) {
      sstate = stater(buffer.line(ss), sstate)
      buffer.setLineTag(ss+1, sstate)
      ss += 1
    }
    sstate
  }

  // /** Indents based on the innermost block that contains pos.
  //   *
  //   *  - Lines following a brace are indented one step from the line that contains the brace.
  //   *    The closing brace is indented to match the line that contains the opening brace.
  //   *
  //   *    void foo () {
  //   *      nextLine();
  //   *    }
  //   *
  //   *  - A line following a dangling open paren is indented one step from the line with the open
  //   *    paren:
  //   *
  //   *    void someFn (
  //   *      a :Int, b :String, c :Double)
  //   *
  //   *  - A line following an open paren which is not the last thing on the line is a continued
  //   *    argument list and is indented to the same column as the open paren:
  //   *
  //   *    void someFn (a :Int, b :String, c :Double,
  //   *                 d :Triple, e :Quadruple)
  //   *
  //   *  - A line following a non-dangling square bracket is indented to the first non-whitespace
  //   *    character following the bracket:
  //   *
  //   *    val foo = [ "one",
  //   *                "two",
  //   *                "three" ]
  //   *    val bar = ["one",
  //   *               "two",
  //   *               "three"]
  //   *
  //   *  - A line following a dangling square bracket is indented as normal block:
  //   *
  //   *    val foo = [
  //   *      "bar",
  //   *      "baz"
  //   *    ]
  //   */

  // /** Indents `bar` and `baz` keywords to match the `foo` keyword for `foo / bar* / baz?` constructs.
  //   * This is generally only needed for `if / else if / else` because the `else if` can repeat.
  //   *
  //   * For `foo / bar? / baz?` constructs (like `try/catch/finally`) just use a pair of
  //   * `PairAnchorAlign` rules `(foo, bar)` and `(foo, baz)`.
  //   */
  // abstract class TripleAnchorAlign (ctx :Context) extends AnchorAlign(ctx) {
}
