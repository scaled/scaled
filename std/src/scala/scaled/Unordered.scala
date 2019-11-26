//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.Collection

/** An unordered finite collection. Elements of these collections may be iterated, but the order in
  * which they are returned is unspecified. When a method indicates that it operates on the
  * elements in iteration order, that order is unspecified.
  *
  * When the concrete class implementing this interface is immutable (i.e. [[List]], [[Seq]],
  * [[Map]], [[Set]]), then the iteration order does not change from one call to the next, but the
  * views of potentially mutable data (i.e. [[SeqV]]) do not make this guarantee.
  *
  * In the case where the concrete type implements [[Ordered]] (a subtype of [[Unordered]]) the
  * iteration order is specified.
  */
abstract class Unordered[+A] extends Iterable[A] {

  /** Returns the number of elements in this collection. */
  def size :Int

  /** Returns true if this collection is empty. */
  def isEmpty :Boolean = (size == 0)

  /** Returns true if this collection is non-empty. */
  def nonEmpty :Boolean = !isEmpty

  override def concat[B >: A] (that :Iterable[B]) :Unordered[B] =
    if (isEmpty && that.isInstanceOf[Unordered[_]]) that.asInstanceOf[Unordered[B]]
    else super.concat(that)

  override def sizeHint = size
}

object Unordered {

  /** Used to build unordered collections. */
  trait Builder[E] {

    /** Builds the target collection, and renders this builder unusable. */
    def build () :Unordered[E]

    /** Appends `value` to this builder. */
    def append (value :E) :this.type
    /** Appends `iter` to this builder. Consumes the iterator in the process. */
    def append (iter :JIterator[_ <: E]) :this.type
    /** Appends `elems` to this builder. */
    def append (elems :JIterable[_ <: E]) :this.type
    /** Appends `elems` to this builder. */
    def append (elems :Unordered[E]) :this.type
    /** Appends `elems` to this builder. */
    def append (elems :SIterable[E]) :this.type

    /** Appends `value` to this builder. */
    def += (value :E) :Unit
    /** See [[append]]. */
    def ++= (iter :JIterator[_ <: E]) :Unit
    /** See [[append]]. */
    def ++= (elems :JIterable[_ <: E]) :Unit
    /** See [[append]]. */
    def ++= (elems :Unordered[E]) :Unit
    /** See [[append]]. */
    def ++= (elems :SIterable[E]) :Unit
  }

  /** Returns an unordered view of `as`. `as` is assumed to be effectively immutable for the
    * lifetime of this view. Violate this assumption at your peril. */
  def view[A] (as :Collection[A]) :Unordered[A] = new Unordered[A]() {
    override def size = as.size
    override def iterator () = as.iterator()
    override def newBuilder[B] (esize :Int) = Seq.builder[B](esize)
    override def newEmpty[B] = Seq.empty
    override protected def toStringType = "UnorderedView"
  }

  private[scaled] final val PartialNotApplied :(Any => Any) = new Function[Any,Any]() {
    def apply (v :Any) = this // need 'this' to point to actual function instance
  }
}
