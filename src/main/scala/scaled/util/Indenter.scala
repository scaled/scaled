//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

import reactual.ValueV
import scala.annotation.tailrec
import scaled._
import scaled.major.CodeConfig

/** Encapsulates a strategy for indenting a line of code. */
abstract class Indenter {

  /** Requests that the indentation for `line` be computed.
    *
    * @param config the configuration for the mode doing the indenting.
    * @param buffer the buffer in which we are indenting.
    * @param block the nearest enclosing block that encloses line.
    * @param line the line to be indented.
    * @param pos position of the first non-whitespace character on `line`.
    *
    * @return `Some(col)` indicating the column to which `line` should be indented, or `None` if
    * this strategy is not appropriate for `line`.
    */
  def apply (config :Config, buffer :BufferV, block :Block, line :LineV, pos :Loc) :Option[Int]
}

object Indenter {
  import Chars._

  /** Returns the number of whitespace chars at the start of `line`. */
  def readIndent (line :LineV) :Int = {
    var pos = 0 ; val end = line.length
    while (pos < end && isWhitespace(line.charAt(pos))) pos += 1
    pos
  }

  /** Returns the number of whitespace chars at the start of the line at `pos`. */
  def readIndent (buffer :BufferV, pos :Loc) :Int = readIndent(buffer.line(pos))

  /** Indents based on the innermost block that contains pos.
    *
    *  - Lines following a brace are indented one step from the line that contains the brace.
    *    The closing brace is indented to match the line that contains the opening brace.
    *
    *    void foo () {
    *      nextLine();
    *    }
    *
    *  - A line following a dangling open paren is indented one step from the line with the open
    *    paren:
    *
    *    void someFn (
    *      a :Int, b :String, c :Double)
    *
    *  - A line following an open paren which is not the last thing on the line is a continued
    *    argument list and is indented to the same column as the open paren:
    *
    *    void someFn (a :Int, b :String, c :Double,
    *                 d :Triple, e :Quadruple)
    *
    *  - A line following a non-dangling square bracket is indented to the first non-whitespace
    *    character following the bracket:
    *
    *    val foo = [ "one",
    *                "two",
    *                "three" ]
    *    val bar = ["one",
    *               "two",
    *               "three"]
    *
    *  - A line following a dangling square bracket is indented as normal block:
    *
    *    val foo = [
    *      "bar",
    *      "baz"
    *    ]
    */
  object ByBlock extends Indenter {
    def apply (config :Config, buffer :BufferV, block :Block, line :LineV, pos :Loc) :Option[Int] = {
      val bstart = block.start
      val openLine = buffer.line(bstart)
      val openNonWS = openLine.find(isNotWhitespace, bstart.col+1)
      val openIsEOL = openNonWS == -1
      val indent = buffer.charAt(bstart) match {
        // align to the first non-whitespace character following the bracket (for non-danglers)
        case '[' if (!openIsEOL) => openNonWS
        // align to the open paren (for non-danglers)
        case '(' if (!openIsEOL) => bstart.col + 1
        // use block indentation for everything else (including {)
        case _ =>
          val openIndent = readIndent(openLine)
          // if the first non-whitespace character is our close brace, use the same indent
          // as the line with the open brace
          if (block.isValid && block.end == pos) openIndent
          // if the block start is the start of the buffer, don't indent
          else if (bstart == buffer.start) 0
          // otherwise indent one from there; TODO: otherwise use previous line's indent?
          else openIndent + config(CodeConfig.indentWidth)
      }
      Some(indent)
    }
  }

  /** Aligns `catch` statements with their preceding `try`. */
  object TryCatchAlign extends PairAnchorAlign("catch", "try")

  /** Aligns `finally` statements with their preceding `try`. */
  object TryFinallyAlign extends PairAnchorAlign("finally", "try")

  /** Aligns `else` and `else if` statements with their preceding `if`. */
  object IfElseIfElseAlign extends TripleAnchorAlign("else", "else if", "if")

  /** Aligns `else` and `elif` statements with their preceding `if`. */
  object IfElifElseAlign extends TripleAnchorAlign("else", "elif", "if")

  /** Aligns `else` statements with their preceding `if`. NOTE: this does not handle `else if`.
    * If your language uses those, use `IfElseIfElse` instead. */
  object IfElseAlign extends PairAnchorAlign("else", "if")

  /** Indents `bar` and `baz` statements to match the `foo` statement for `foo / bar* / baz?`
    * constructs. This is generally only needed for `if / else if / else` because the `else if` can
    * repeat. For `foo / bar? / baz?` constructs (like `try/catch/finally`) just use a pair of
    * `PairAnchorAlign` rules `(foo, bar)` and `(foo, baz)`.
    */
  class TripleAnchorAlign (last :String, middle :String, anch :String) extends AnchorAlign(anch) {
    protected val middleM = Matcher.exact(middle)
    protected val lastM = Matcher.exact(last)
    protected val anchors = List(middleM, anchorM)

    def apply (config :Config, buffer :BufferV, block :Block, line :LineV, pos :Loc) :Option[Int] = {
      if (!line.matches(lastM, pos.col) && !line.matches(middleM, pos.col)) None
      else indentToAnchors(buffer, block, pos, anchors)
    }
  }

  /** Indents `bar` statements to match their `foo` counterpart. Examples of `foo/bar` may include
    * `if/else`, `try/catch`, `try/finally`, `for/yield`. NOTE: this should not be used for
    * `if/else if/else` because the `else if` can repeat, which this won't handle. Use
    * `TripleAnchorAlign` for that case.
    */
  class PairAnchorAlign (second :String, anch :String) extends AnchorAlign(anch) {
    protected val secondM = Matcher.exact(second)

    def apply (config :Config, buffer :BufferV, block :Block, line :LineV, pos :Loc) :Option[Int] = {
      if (!line.matches(secondM, pos.col)) None
      else indentToAnchor(buffer, block, pos)
    }
  }

  /** Indents statements relative to an anchor statement. This is chiefly useful for things like
    * aligning an `else` with the `if` that preceded it, etc.
    *
    * @param anchor the keyword that identifies the anchor statement.
    */
  abstract class AnchorAlign (anchor :String) extends Indenter {
    protected val anchorM = Matcher.exact(anchor)

    protected def indentToAnchor (buffer :BufferV, block :Block, pos :Loc) :Option[Int] =
      buffer.findBackward(anchorM, pos, block.start) match {
        case Loc.None => None
        case loc      => Some(loc.col)
      }

    @tailrec protected final def indentToAnchors (buffer :BufferV, block :Block, pos :Loc,
                                                  anchors :List[Matcher]) :Option[Int] =
      if (anchors.isEmpty) None
      else buffer.findBackward(anchors.head, pos, block.start) match {
        case Loc.None => indentToAnchors(buffer, block, pos, anchors.tail)
        case loc      => Some(loc.col)
      }
  }
}
