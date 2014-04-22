//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import reactual.ValueV
import scala.annotation.tailrec
import scaled._
import scaled.util.{Blocker, Block, Chars}

/** Handles indenting code for a [[CodeMode]].
  */
class Indenter (buffer :BufferV, blocker :Blocker, indentWidth :ValueV[Int]) {
  import Chars._

  /** Encapsulates a strategy for indenting a line of code. */
  trait IndentFn {

    /** Attempts to indent `line`.
      *
      * @param line the line to be indented.
      * @param pos position of the first non-whitespace character on `line`.
      * @param block the nearest enclosing block that encloses line.
      *
      * @return `Some(col)` indicating the column to which `line` should be indented, or `None` if
      * this strategy is not appropriate for `line`.
      */
    def apply (line :LineV, pos :Loc, block :Block) :Option[Int]
  }

  /** A list of indentation functions which will be applied in order to determine the indentation for
    * a line. The first fn to return `Some(col)` is used. */
  val indentFns :List[IndentFn] = List(
    new IndentOneLiner(List("else", "else if", "if")),
    IndentByBlock
  )

  /** Returns the number of whitespace chars at the start of `line`. */
  def readIndent (line :LineV) :Int = {
    var pos = 0 ; val end = line.length
    while (pos < end && isWhitespace(line.charAt(pos))) pos += 1
    pos
  }

  /** Returns the number of whitespace chars at the start of the line at `pos`. */
  def readIndent (pos :Loc) :Int = readIndent(buffer.line(pos))

  /** Computes the indentation for the line at `row`. */
  def computeIndent (row :Int) :Int = {
    // find the position of the first non-whitespace character on the line
    val line = buffer.line(row)
    val wsp = line.find(isNotWhitespace)
    val start = Loc(row, if (wsp == -1) 0 else wsp)
    val block = blocker(start, 0) getOrElse Block(buffer.start, buffer.end, true)
    @tailrec @inline def loop (fns :List[IndentFn]) :Int =
      if (fns.isEmpty) 0 else {
        val opt = fns.head(line, start, block)
        if (opt.isDefined) opt.get else loop(fns.tail)
      }
    loop(indentFns)
  }

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
  object IndentByBlock extends IndentFn {
    def apply (line :LineV, pos :Loc, block :Block) :Option[Int] = {
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
          // otherwise indent one from there
          else openIndent + indentWidth()
      }
      Some(indent)
    }
  }

  /** Indents bare one liner statements like `if/else if/else` or `try/catch`.
    *
    * @param keywords a list of keywords that identify the one liner statements, in reverse order
    * of occurrence. For example: `List("else", "else if", "if")`. If a line starts with a keyword
    * in the list, it will be aligned to the first keyword after it in the list that can be found,
    * up to the bounds of the current block. Thus `else` will align to `else if` or `if`, but `else
    * if` will only align to `if`.
    */
  protected class IndentOneLiner (keywords :List[String]) extends IndentFn {
    private val matchers = keywords.map(Matcher.exact)

    def apply (line :LineV, pos :Loc, block :Block) :Option[Int] = {
      // seeks one of the tokens in ms prior to pos and returns its column if found
      @inline @tailrec def indent (ms :List[Matcher]) :Option[Int] = {
        if (ms.isEmpty) None
        else buffer.findBackward(ms.head, pos, block.start) match {
          case Loc.None => indent(ms.tail)
          case loc      => Some(loc.col)
        }
      }
      // if we see one of our keywords, look backward for one of the matching keywords and indent
      // to its column if we find one
      @inline @tailrec def loop (ms :List[Matcher]) :Option[Int] = {
        if (line.matches(ms.head, pos.col)) indent(ms.tail)
        else if (ms.size > 2) loop(ms.tail)
        else None
      }
      loop(matchers)
    }
  }
}
