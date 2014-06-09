//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman

import java.io.PrintStream

class Printer (out :PrintStream, val indent :String = "") {

  def nest (indent :String) = new Printer(out, this.indent+indent)

  def println (text :String) = out.println(text)

  def printCols (cols :Iterable[(String,String)], onEmpty :String, gap :Int = 2) {
    if (cols.iterator.hasNext) {
      val c1wid = cols.map(_._1.length).max + gap
      cols foreach {
        case (c1, c2) =>
          out.print(c1)
          pad(c1wid-c1.length)
          out.println(c2)
      }
    } else println(onEmpty)
  }

  private def pad (count :Int) :Unit =
    if (count > 0) { out.print(" ") ; pad(count-1) }
}
