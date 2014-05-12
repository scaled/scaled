//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.util.{Chars, Filler}

/** A helper class for dealing with comments in source code: wrapping, filling, etc. */
class Commenter (buffer :BufferV) {
  import CodeConfig._
  import Chars._

  /** The string to prepend to an auto-filled line comment. */
  def commentPrefix :String = ""
  /** A matcher on [[commentPrefix]]. */
  val commentPrefixM = Matcher.exact(commentPrefix)

  /** The string to prepend to an auto-filled doc line. */
  def docPrefix :String = ""
  /** A matcher on [[docPrefix]]. */
  val docPrefixM = Matcher.exact(docPrefix)

  /** Returns the text to be prepended to a comment about to be auto-wrapped at `at`. */
  def prefixFor (at :Loc) = {
    val ss = buffer.stylesNear(at)
    // TODO: count the number of spaces after the comment prefix on the current line and
    // duplicate that instead of assuming one space
    if (ss contains commentStyle) commentPrefix + " "
    else if (ss contains docStyle) docPrefix + " "
    else ""
  }

  /** Returns true if `ss` contains `docStyle` or `commentStyle`. */
  def isCommentStyle (ss :Styles) :Boolean = (ss contains commentStyle) || (ss contains docStyle)

  /** Returns true if `p` is "inside" a comment. The default implementation checks whether `p` is
    * styled with `commentFace` or `docFace`, or if `p` is the end if its line, whether the
    * character preceding `p` is thusly styled.
    */
  def inComment (p :Loc) :Boolean = isCommentStyle(buffer.stylesNear(p))

  /** Returns the column of start of the comment on `line`. If `line` does not contain comments,
    * `line.length` is returned. */
  def commentStart (line :LineV) :Int = {
    val llen = line.length ; var c = 0
    // first skip over non-comments at the start of the line
    while (c < llen && !isCommentStyle(line.stylesAt(c))) c += 1
    // next skip whitespace
    while (c < llen && isWhitespace(line.charAt(c))) c += 1
    // next skip over the comment or doc prefix and we're done
    c += commentDelimLen(line, c)
    // finally skip whitespace again, and we're done
    while (c < llen && isWhitespace(line.charAt(c))) c += 1
    c
  }

  /** Returns the length of the comment delimiter at `col` of `line`. Returns 0 if no comment
    * delimiter is matched at `col`. This is used to skip over the comment delimiter when scanning
    * for comment start. The default implementation checks for [[commentPrefix]] or [[docPrefix]].
    */
  def commentDelimLen (line :LineV, col :Int) :Int =
    if (line.matches(commentPrefixM, col)) commentPrefixM.matchLength
    else if (line.matches(docPrefixM, col)) docPrefixM.matchLength
    else 0

  /** Generates a comment prefix given the supplied desired comment start column. This combines the
    * appropriate number of spaces with `commentPre` and a single trailing space.
    */
  def commentPre (commentPre :String, startCol :Int) :Line = {
    val spaces = " " * (startCol - commentPre.length - 1)
    new Line(spaces + commentPrefix + " ") // TODO: infer number of trailing spaces?
  }

  /** Refills the comments region `[start, end)`. `start` may be a line which contains some
    * non-comment text prior to the comment, but subsequent lines must have only whitespace
    * preceding the start of their comments.
    * @return the refilled comments region.
    */
  def refillComments (fillColumn :Int, start :Loc, end :Loc) :Seq[Line] = {
    println(s"refillLinesIn($start, $end)")
    // the first line dictates the prefix width and fill width
    val firstLine = buffer.line(start)
    val firstCol = commentStart(firstLine)
    // the first line's prefix is preserved as is, for the second+ lines we use a "repeat" prefix
    val firstPre = firstLine.view(start.col, firstCol)
    val repeatPre = {
      // if we have more than one row in our comment block, just use the second line's prefix as our
      // repeat prefix because it's most likely to be correct
      if (end.row > start.row) buffer.line(start.nextStart).view(0, firstPre.length)
      // otherwise create a prefix using `commentPrefix` (using "" if we lack one)
      else commentPre(commentPrefix, firstPre.length)
    }

    // append all of the to-be-filled text to a filler configured with our wrap width
    val filler = new Filler(fillColumn - firstCol)
    filler.append(firstLine.view(firstCol, firstLine.length))
    var loc = start.nextStart ; while (loc < end) {
      val line = buffer.line(loc)
      val last = if (loc.row == end.row) end.col else line.length
      filler.append(line.view(commentStart(line), last))
      loc = loc.nextStart
    }

    // now prepend the appropriate prefix back onto each filled line and replace the original
    // buffer region with our new wrapped results
    val filled = filler.result
    var pres = Seq(firstPre) ++ Seq.fill(filled.length-1)(repeatPre)
    pres zip filled map { case (pre, line) => pre merge line }
  }

}
