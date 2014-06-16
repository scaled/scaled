//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

/** A helper class used to identify paragraphs. */
class Paragrapher (val syntax :Syntax, buffer :Buffer) {

  /** Returns the paragraph that surrounds `loc` if one can be identified. */
  def paragraphAt (loc :Loc) :Option[Region] = {
    // scan backward for the first non-delim line
    var row = loc.row
    while (row >= 0 && isDelim(row)) row -= 1
    // if we couldn't find one, then we have no paragraph
    if (row < 0) None
    else {
      // now extend up as much as we can, and below as much as we can
      var first = row
      while (first > 0 && canPrepend(first-1)) first -= 1
      val max = buffer.lines.length-1 ; var last = row
      while (last < max && canAppend(last+1)) last += 1
      // if our paragraph extends to the last line of the buffer, we have to bound at the end of
      // that line rather than the start of the next line
      val end = if (last == max) Loc(last, buffer.line(last).length) else Loc(last+1, 0)
      Some(Region(Loc(first, 0), end))
    }
  }

  def line (row :Int) = buffer.line(row)
  def isDelim (row :Int) = line(row).length == 0
  def canPrepend (row :Int) = !isDelim(row)
  def canAppend (row :Int) = !isDelim(row)
}
