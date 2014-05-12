//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.code

import java.util.Arrays
import reactual.OptValue
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._

/** Tracks a block of code: a region delimited by an open bracket and close bracket. The `start` of
  * the region will point to the open bracket character, and the `end` of the region will point to
  * the close bracket character. See [[isValid]] for a caveat with regard to the end of the region.
  *
  * @param isValid whether or not this block is correctly closed. If not, [[end]] will point to the
  * mismatched close bracket, or the end of the buffer.
  */
case class Block (start :Loc, end :Loc, isValid :Boolean) extends Region {
  override def toString = (if (isValid) "" else "!") + Region.toString(start, end)
}

/** Handles identifying code [[Block]]s.
  */
class Blocker (buffer :RBuffer, openers :String, closers :String) {

  /** Returns the inner-most block that encloses `loc`. The block will be restricted to characters
    * that have the same syntax class as the character at `loc. See [[classify]]. */
  def apply (loc :Loc) :Option[Block] = apply(loc, classify(loc.row, loc.col))

  /** Returns the inner-most block that encloses `loc` and which has the specified syntax class.
    * See [[classify]]. */
  def apply (loc :Loc, clazz :Int) :Option[Block] = {
    val srow = loc.row ; val scol = loc.col
    findOpener.reset(clazz)
    findCloser.reset(clazz)
    // if the character immediately prior to `loc` is a close bracket; return that block
    val pcidx = if (scol == 0 || classify(srow, scol-1) != clazz) -1
                else closers.indexOf(buffer.charAt(loc.prevC))
    if (pcidx >= 0) {
      val start = buffer.scanBackward(findOpener, loc.prevC)
      val sbidx = openers.indexOf(buffer.charAt(start))
      if (sbidx == pcidx) Some(Block(start, loc.prevC, true))
      else None
    }
    // otherwise scan backwards for the first opener (skipping matched pairs along the way)
    else {
      val start = buffer.scanBackward(findOpener, loc)
      val sbidx = openers.indexOf(buffer.charAt(start))
      // we may have hit the start of the buffer and seen no opener
      if (sbidx == -1) None
      else {
        val end = buffer.scanForward(findCloser, loc)
        val ebidx = closers.indexOf(buffer.charAt(end))
        Some(Block(start, end, sbidx == ebidx))
      }
    }
  }

  /** Provides a way for modes to define syntax classes for different parts of the buffer. This is
    * mainly useful for differentiating between comments and strings so that brackets in one type
    * of syntax do not match brackets in a different type. */
  protected def classify (row :Int, col :Int) :Int = 0

  // scans backwards, looking for an unmatched opener
  class Scanner (starts :String, ends :String) extends Function3[Int,Int,Char,Boolean] {
    val counts = new Array[Int](starts.length)
    val zeros = counts.clone()
    private[this] var clazz = 0

    def apply (row :Int, col :Int, c :Char) :Boolean = {
      // if we see a block starter, tick up a counter for that bracket
      val sidx = starts.indexOf(c)
      if (sidx >= 0 && classify(row, col) == clazz) counts(sidx) += 1
      else {
        // if we see a block ender, tick down the counter for that bracket
        val eidx = ends.indexOf(c)
        if (eidx >= 0 && classify(row, col) == clazz) {
          val cur = counts(eidx)
          // if we haven't seen a starter for this opener, it's what we're looking for
          if (cur == 0) return true
          else counts(eidx) = cur-1
        }
      }
      false
    }

    // resets this scanner and prepares it for operation
    def reset (clazz :Int) {
      this.clazz = clazz
      Arrays.fill(counts, 0)
    }

    // we're valid if we find an unmatched opener and have no pending closers
    def isValid () = Arrays.equals(counts, zeros)
  }
  private val findOpener = new Scanner(closers, openers)
  private val findCloser = new Scanner(openers, closers)
}
