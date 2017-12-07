//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{AbstractSet, Collection, Comparator, Set => JSet}

/** An unordered collection of elements, with no duplicates. */
abstract class Set[A] extends Unordered[A] with (A => Boolean) {

  /** An alias for [[contains]] which allows a set to act as `(A => Boolean)`. */
  def apply (elem :A) :Boolean = contains(elem)

  /** Returns true if this set contains `elem`, per [[Objects.equals]]. */
  def contains (elem :A) :Boolean

  /** Returns the union of `this` set and `that` set. */
  def union (that :Set[A]) :Set[A] = {
    val sb = Set.builder[A](size+that.size)
    sb ++= this
    sb ++= that
    sb.build()
  }
  /** An alias for [[union]]. */
  def | (that :Set[A]) :Set[A] = this union that

  /** Returns the intersection of `this` set and `that` set. */
  def intersect (that :Set[A]) :Set[A] = {
    val sb = Set.builder[A](size)
    val iter = iterator() ; while (iter.hasNext) {
      val a = iter.next
      if (that(a)) sb += a
    }
    sb.build()
  }
  /** An alias for [[intersect]]. */
  def & (that :Set[A]) :Set[A] = this intersect that

  /** Returns the difference of `this` set and `that` set. */
  def diff (that :Set[A]) :Set[A] = {
    val sb = Set.builder[A](size)
    val iter = iterator() ; while (iter.hasNext) {
      val a = iter.next
      if (!that(a)) sb += a
    }
    sb.build()
  }
  /** An alias for [[diff]]. */
  def &~ (that :Set[A]) :Set[A] = this diff that
  /** An alias for [[diff]]. */
  def -- (that :Set[A]) :Set[A] = this diff that

  /** Returns `this` set with `elem` removed. NOTE: this is expensive. Scaled sets are not
    * persistent data structures, so the entire set is duplicated, minus the specified element. */
  def remove (elem :A) :Set[A] = if (!contains(elem)) this else {
    val sb = Set.builder[A](size)
    val iter = iterator() ; while (iter.hasNext) {
      val a = iter.next
      if (a != elem) sb += a
    }
    sb.build()
  }
  /** An alias for [[remove]]. */
  def - (elem :A) :Set[A] = remove(elem)

  // views
  /** Returns a view of this seq as a [[JList]]. */
  def asJSet :JSet[A] = new AbstractSet[A]() {
    override def size = Set.this.size
    override def contains (elem :Any) = Set.this.contains(elem.asInstanceOf[A])
    override def iterator = Set.this.iterator
  }

  // overrides for performance and type specificity
  override def concat[B >: A] (that :Iterable[B]) :Set[B] = super.concat(that).toSet
  override def ++[B >: A] (bs :Iterable[B]) :Set[B] = concat(bs)

  override def filter (pred :A => Boolean) :Set[A] = super.filter(pred).toSet
  override def filterNot (pred :A => Boolean) :Set[A] = super.filterNot(pred).toSet
  override def flatMap[B] (f :A => JIterable[B]) :Set[B] = super.flatMap(f).toSet
  override def foldBuild[B] (op :(Unordered.Builder[B],A) => Unit) :Set[B] =
    super.foldBuild(op).toSet
  override def map[B] (f :A => B) :Set[B] = super.map(f).toSet

  override def toSet[B >: A] :Set[B] = this.asInstanceOf[Set[B]]

  override def newBuilder[B] (expectedSize :Int) = Set.builder[B](expectedSize)
  override def newEmpty[B] = Set.empty
  override protected def toStringType = "Set"

  // prevent Function1 from usurping our toString
  override def toString = super[Unordered].toString

  override def equals (that :Any) :Boolean = that match {
    case oset :Set[A] => (oset.size == size) && (this forall oset)
    case _ => false
  }
}

object Set {

  /** Used to build [[Set]]s. */
  trait Builder[A] extends Unordered.Builder[A] {
    override def build () :Set[A]

    /** Builds a sorted set using the element's natural ordering.
      * The elements must implement [[Comparable]]. */
    def buildSorted () :Set[A]
    /** Builds a sorted set using `cmp` to order the elements. */
    def buildSorted (cmp :Comparator[_ >: A]) :Set[A]
    /** Builds a sorted set using `ord` to order the elements. */
    def buildSorted (implicit ord :Ordering[_ >: A]) :Set[A] = buildSorted(ord :Comparator[_ >: A])
  }

  /** Returns a [[Set]] builder. */
  def builder[A] () :Builder[A] = builder(4)
  /** Returns a [[Set]] builder prepared to build a set with at least `expectedSize` elements. */
  def builder[A] (expectedSize :Int) :Builder[A] = new OpenHashSet.Builder(expectedSize)

  /** Returns the empty set. */
  def empty[A] :Set[A] = EMPTY.asInstanceOf[Set[A]]

  /** Returns the empty set. */
  def apply[A] () :Set[A] = empty
  /** Returns a one element set containing `e0`. */
  def apply[A] (e0 :A) :Set[A] =
    builder[A](1).append(e0).build()
  /** Returns a two element set containing `{e0, e1}`. */
  def apply[A] (e0 :A, e1 :A) :Set[A] =
    builder[A](2).append(e0).append(e1).build()
  /** Returns a three element set containing `{e0, e1, e2}`. */
  def apply[A] (e0 :A, e1 :A, e2 :A) :Set[A] =
    builder[A](3).append(e0).append(e1).append(e2).build()
  /** Returns a four element set containing `{e0, e1, e2, e3}`. */
  def apply[A] (e0 :A, e1 :A, e2 :A, e3 :A) :Set[A] =
    builder[A](4).append(e0).append(e1).append(e2).append(e3).build()
  /** Returns a set containing `{e0, e1, e2, e3, rest...}`. */
  def apply[A] (e0 :A, e1 :A, e2 :A, e3 :A, rest :A*) :Set[A] =
    builder[A](4+rest.size).append(e0).append(e1).append(e2).append(e3).append(rest).build()

  /** Returns a set containing the elements of `as`. */
  def copyOf[A] (as :Collection[A]) :Set[A] = builder[A](as.size).append(as).build()
  /** Returns a set containing the elements of `as`. */
  def copyOf[A] (as :Unordered[A]) :Set[A] = builder[A](as.size).append(as).build()

  // we box primitive arrays up front, rather than leaving them as is and unboxing every time an
  // element is accessed (which is how stock Scala collections do things)
  import BoxUtil._

  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Boolean]) :Set[Boolean] = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Byte])    :Set[Byte]    = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Char])    :Set[Char]    = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Short])   :Set[Short]   = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Int])     :Set[Int]     = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Long])    :Set[Long]    = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Float])   :Set[Float]   = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. */
  def from (elems :Array[Double])  :Set[Double]  = mkSet(box(elems), elems.length)
  /** Returns a [[Set]] containing `elems`. Ownership of `elems` is taken by the created set. */
  def from[A <: AnyRef] (elems :Array[A]) :Set[A] = mkSet(
    elems.asInstanceOf[Array[Any]], elems.length)

  private def mkSet[A] (elems :Array[Any], size :Int) :Set[A] =
    if (size == 0) empty else OpenHashSet.make(elems, size)

  /** Returns a view of the Java `set` as a [[Set]]. `set` is assumed to be effectively immutable
    * for the lifetime of this view. Violate this assumption at your peril. */
  def view[A] (set :JSet[A]) :Set[A] = new Set[A]() {
    override def size = set.size
    override def contains (elem :A) = set.contains(elem)
    override def iterator () = set.iterator()
  }

  private final val EMPTY = new Set[Any]() {
    override def size = 0
    override def contains (elem :Any) = false
    override def iterator () = Iterables.EMPTY_ITER.asInstanceOf[JIterator[Any]]
  }
}
