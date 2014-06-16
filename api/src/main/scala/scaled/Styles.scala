//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.util.{List => JList}
import scala.annotation.tailrec

/** Encapsulates a set of zero or more CSS style classes. This is effectively a set of strings with
  * a more limited API, and with care taken to ensure that for every combination of styles, one
  * canonical `Styles` instance exists. Due to the way Scaled maintains style information, this is
  * memory efficient and it allows for fast comparison.
  *
  * In general, there are a large number of potential style combinations, but in practice a single
  * character in a buffer is rarely styled with more than two or three styles, which means we only
  * ever have a few hundred unique instances.
  */
final class Styles private (styles :Array[String]) {

  /** Returns true if `style` is contained in this set of styles. */
  def contains (style :String) :Boolean = {
    @tailrec def contains (ii :Int) :Boolean = (ii >= 0) && (styles(ii) == style || contains(ii-1))
    contains(styles.length-1)
  }

  /** Applies `op` to all styles in this instance. */
  def foreach[U] (op :String => U) :Unit = {
    @inline @tailrec def apply (ii :Int) { if (ii >= 0) { op(styles(ii)) ; apply(ii-1) }}
    apply(styles.length-1)
  }

  /** Adds all all styles to `list`. This is used to add styles to a JavaFX `Node`. */
  def addTo (list :JList[String]) {
    @inline @tailrec def add (ii :Int) { if (ii >= 0) { list.add(styles(ii)) ; add(ii-1) }}
    add(styles.length-1)
  }

  /** Returns a new styles instance which adds `style` to this set of styles. */
  def + (style :String) = if (contains(style)) this else Styles.add(styles, style)

  /** Returns a new styles instance which removes `style` from this set of styles. */
  def - (style :String) = if (contains(style)) Styles.remove(styles, style) else this

  /** Returns a new style instance with all styles which match `pred` removed. */
  def - (pred :String => Boolean) = Styles.remove(styles, pred)

  override def toString = styles.mkString(" ")

  private def extend (style :String) = new Styles(styles :+ style)
}

object Styles {
  import java.util.function.{Function => JFunction}
  import java.util.concurrent.ConcurrentHashMap

  private val EmptyArray = Array[String]()

  /** The `Styles` instance with no styles. */
  val None = new Styles(EmptyArray)

  /** Returns the `Styles` instance with no styles. */
  def apply () :Styles = None

  /** Returns a `Styles` instance with the single style `style`. */
  def apply (style :String) = add(EmptyArray, style)

  private def add (styles :Array[String], style :String) :Styles = {
    @tailrec def loop (ii :Int, usedStyle :Boolean, node :Node) :Node = {
      if (ii == styles.length) if (usedStyle) node else node.child(style)
      else {
        val s = styles(ii)
        if (usedStyle || style > s) loop(ii+1, usedStyle, node.child(s))
        else loop(ii, true, node.child(style))
      }
    }
    loop(0, false, _root).styles
  }

  private def remove (styles :Array[String], style :String) :Styles = {
    @tailrec def loop (ii :Int, node :Node) :Node = {
      if (ii == styles.length) node
      else {
        val s = styles(ii)
        if (s == style) loop(ii+1, node)
        else loop(ii+1, node.child(s))
      }
    }
    loop(0, _root).styles
  }

  private def remove (styles :Array[String], pred :String => Boolean) :Styles = {
    @tailrec def loop (ii :Int, node :Node) :Node = {
      if (ii == styles.length) node
      else {
        val s = styles(ii)
        if (pred(s)) loop(ii+1, node)
        else loop(ii+1, node.child(s))
      }
    }
    loop(0, _root).styles
  }

  class Node (val styles :Styles) {
    def child (style :String) :Node = _kids.computeIfAbsent(style, _compute)
    private[this] val _compute = new JFunction[String,Node]() {
      def apply (style :String) = new Node(styles.extend(style))
    }
    private[this] lazy val _kids = new ConcurrentHashMap[String,Node]()
  }
  private val _root = new Node(None)
}
