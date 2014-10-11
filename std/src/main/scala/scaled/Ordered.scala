//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.function.BiConsumer
import java.util.{Arrays, Collection, Comparator, HashSet, Objects}
import scala.reflect.ClassTag

/** An ordered finite collection. This has two main concrete incarnations: [[List]] and [[Seq]]. */
abstract class Ordered[+A] extends Unordered[A] {
  import Ordered.Builder

  /** Returns the number of elements in this ordered collection. This is an alias for [[size]]
    * because sometimes you feel like a nut, and sometimes you don't.*/
  def length :Int = size

  /** Finds the first element for which `pf` is defined and applies `pf` to it. */
  def collectFirst[B] (pf :PartialFunction[A,B]) :Option[B] = {
    val iter = iterator() ; while (iter.hasNext) {
      val res = pf.applyOrElse(iter.next, Unordered.PartialNotApplied)
      if (res.asInstanceOf[AnyRef] ne Unordered.PartialNotApplied) return Some(res.asInstanceOf[B])
    }
    None
  }

  /** Returns true if `this` contains `elem`, per [[Objects.equals]]. */
  def contains[B >: A] (elem :B) :Boolean = {
    val iter = iterator()
    while (iter.hasNext()) if (Objects.equals(elem, iter.next)) return true
    return false
  }

  /** Copies `[start, end)` from this collection into `target` at `offset`.
    * @throws IndexOutOfBoundsException if start or end are out of bounds.
    * @throws ArrayIndexOutOfBoundsException if target does not contain enough space at offset to
    * hold the requested slice.
    */
  def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) :Unit

  /** Returns the distinct elements of `as`, in order of first appearance. `A` must
    * implement the `hashCode` and `equals` contract. */
  def distinct () :Ordered[A] = {
    val das = newBuilder[A]()
    val seen = new HashSet[A]()
    val iter = iterator() ; while (iter.hasNext) {
      val elem = iter.next
      if (seen.add(elem)) das.append(elem)
    }
    das.build()
  }

  /** Returns all remaining elements after `count` elements have been skipped.
    * If fewer than `count` elements exist, an empty collection is returned. */
  def drop (count :Int) :Ordered[A] = if (count <= 0) this else {
    val size = this.size
    if (count >= size) newEmpty
    else uncheckedSlice(count, size)
  }

  /** Returns all but the last `count` elements. If fewer than `count` elements exist, an empty
    * collection is returned. */
  def dropRight (count :Int) :Ordered[A] = if (count <= 0) this else {
    val size = this.size
    if (count >= size) newEmpty
    else uncheckedSlice(0, size-count)
  }

  /** Drops the longest prefix that satisfies `pred`. */
  def dropWhile (pred :A => Boolean) :Ordered[A] = {
    var ii = 0 ; val iter = iterator() ; while (iter.hasNext && pred(iter.next)) ii += 1
    if (ii == size) newEmpty else uncheckedSlice(ii, size)
  }

  /** Returns true if this collection ends with elements equal to those in `suffix`, per
    * [[Objects.equals]]. */
  def endsWith[B >: A] (suffix :Ordered[B]) :Boolean = {
    val tsize = this.size ; val ssize = suffix.size
    if (ssize > tsize) false
    else {
      val iter = this.iterator() ; val siter = suffix.iterator()
      var ii = 0 ; val ll = tsize-ssize ; while (ii < ll) { iter.next ; ii += 1 }
      while (iter.hasNext()) if (!Objects.equals(iter.next, siter.next)) return false
      true
    }
  }

  /** Applies `f` to each element of this list and returns a collection that contains the
    * "concatenation" of the results. */
  def flatMap[B] (f :A => Iterable[B]) :Ordered[B] = foldBuild[B]((b, a) => b ++= f(a))

  /** Folds `op` over this collection, from left to right. */
  def foldLeft[B] (zero :B)(op :(B,A) => B) :B = fold(zero)(op)
  /** Folds `op` over this collection, from left to right. */
  @inline final def /:[B] (zero :B)(op :(B,A) => B) :B = fold(zero)(op)

  /** Partitions this collection into fixed size groups.
    * Each group (excepting possibly the last) will have `size` elements. */
  def grouped (size :Int) :Ordered[Ordered[A]] = {
    val bb = newBuilder[Ordered[A]]()
    val iter = iterator()
    while (iter.hasNext) {
      val gb = newBuilder[A](size)
      var ii = 0 ; while (ii < size && iter.hasNext) { gb += iter.next ; ii += 1 }
      bb += gb.build()
    }
    bb.build()
  }

  /** Returns the first element of this collection.
    * @throws NoSuchElementException if called on an empty collection. */
  def head :A = iterator().next

  /** Returns an option containing the first element of this collection, or `None`. */
  def headOption :Option[A] = if (isEmpty) None else Some(head)

  /** Returns last element of this collection.
    * @throws NoSuchElementException if called on an empty collection. */
  def last :A = {
    val iter = iterator()
    if (!iter.hasNext) throw new NoSuchElementException("'last' called on empty collection")
    var last = iter.next ; while (iter.hasNext) last = iter.next
    last
  }

  /** Returns an option containing the last element of this collection, or `None`. */
  def lastOption :Option[A] = if (isEmpty) None else Some(last)

  /** Reduces this collection, from left to right, via `op`.
    * @throws NoSuchElementException when called on an empty collection. */
  def reduceLeft[B >: A] (op :(B,A) => B) :B = reduce(op)

  /** Returns a copy of this collection, sorted via `cmp`. */
  def sorted (cmp :Comparator[_ >: A]) :Ordered[A] = {
    val array = new Array[Any](size)
    copyInto(0, array.length, array, 0)
    Arrays.sort[A](array.asInstanceOf[Array[A with Object]], cmp)
    build(array, array.length)
  }
  /** Returns a copy of this collection, sorted via `cmp`. */
  def sorted (implicit cmp :Ordering[_ >: A]) :Ordered[A] = sorted(cmp :Comparator[_ >: A])

  /** Returns a copy of this collection, sorted via `fn` and `cmp`. */
  def sortBy[B] (fn :(A => B), cmp :Comparator[B]) :Ordered[A] = sorted(new Comparator[A]() {
    override def compare (a0 :A, a1 :A) = cmp.compare(fn(a0), fn(a1))
  })
  /** Returns a copy of this collection, sorted via `fn` and `cmp`. */
  def sortBy[B] (fn :(A => B))(implicit cmp :Ordering[B]) :Ordered[A] = sorted(cmp on fn)

  /** Returns the slice of this seq in the range `[from, until)`.
    * @throws IndexOfBoundsException if either end of the slice is out of bounds. */
  def slice (from :Int, until :Int) :Ordered[A] = {
    val size = this.size
    Seq.checkBounds(from, until, size)
    if (from == until) newEmpty else uncheckedSlice(from, until)
  }

  /** Returns the first `count` elements of this collection. If this collection is `count` or fewer
    * elements in length, `this` is returned. */
  def take (count :Int) :Ordered[A] =
    if (count <= 0) newEmpty
    else if (size <= count) this
    else uncheckedSlice(0, count)

  /** Returns the last `count` elements. If this collection is `count` or fewer elements in length,
    * `this` is returned. */
  def takeRight (count :Int) :Ordered[A] =
    if (count <= 0) newEmpty
    else {
      val size = this.size
      if (size <= count) this
      else uncheckedSlice(size-count, size)
    }

  /** Takes the longest prefix that satisfies `pred`. */
  def takeWhile (pred :A => Boolean) :Ordered[A] = {
    var ii = 0 ; val iter = iterator() ; while (iter.hasNext && pred(iter.next)) ii += 1
    if (ii == 0) newEmpty else uncheckedSlice(0, ii)
  }

  /** Returns a filtered view of this collection. **Note**: the view is lazily computed. This
    * chiefly exists to support Scala's for comprehension syntax, but cal also be used directly if
    * one wants to avoid creating an intermediate collection when doing `map`, `flatMap` or
    * `foreach` on the filtered collection. */
  def withFilter (pred :A => Boolean) :Filtered[A] = new Filtered[A](this, pred) {
    override protected def newBuilder[B] () = Ordered.this.newBuilder[B]()
  }

  /** Zips `this` with `that`. The result will contain `(a0,b0), (a1,b1), ...` and will have length
    * equal to that of the shorter of `this` and `that`. */
  def zip[B] (that :Ordered[B]) :Ordered[(A,B)] = {
    val zb = newBuilder[(A,B)](math.min(size, that.size))
    val aiter = iterator() ; val biter = that.iterator()
    while (aiter.hasNext && biter.hasNext) zb.append((aiter.next, biter.next))
    zb.build()
  }

  /** Zips `this` with `that`. The result will contain `(a0,b0), (a1,b1), ...` and will "pad" the
    * shorter collection with either `a` or `b` as appropriate. The returned collection will thus
    * have length equal to that of the longer of `this` and `that`. */
  def zipAll[A1 >: A,B] (that :Ordered[B], a :A1, b :B) :Ordered[(A1,B)] = {
    val zb = newBuilder[(A1,B)](math.max(size, that.size))
    val aiter = iterator() ; val biter = that.iterator()
    while (aiter.hasNext || biter.hasNext) {
      val aa = if (aiter.hasNext) aiter.next else a
      val bb = if (biter.hasNext) biter.next else b
      zb.append((aa, bb))
    }
    zb.build()
  }

  /** Zips this collection with the indices of its elements. */
  def zipWithIndex :Ordered[(A,Int)] = {
    val zb = newBuilder[(A,Int)](size)
    val iter = iterator() ; var ii = 0
    while (iter.hasNext) { zb.append((iter.next, ii)) ; ii += 1 }
    zb.build()
  }

  /** Creates and returns an array containing the contents of this collection. Scala compiler magic
    * (i.e. `ClassTag`) is used to determine the appropriate element type for the array.
    *
    * For reasons of efficiency this is only supported for collections of reference types. You
    * cannot turn a `Set[Int]` into an `Array[Int]`. That would require unboxing each element in
    * turn and inserting it into the target array. Instead we want to always copy using
    * `System.arraycopy`. */
  def toArray[B >: A <: AnyRef :ClassTag] :Array[B] = {
    val result = new Array[B](size)
    copyInto(result.asInstanceOf[Array[Any]], 0)
    result
  }

  // type &c specializations
  override def copyInto (target :Array[Any], offset :Int) :Unit =
    copyInto(0, size, target, offset)
  override def filter (pred :A => Boolean) :Ordered[A] =
    super.filter(pred).asInstanceOf[Ordered[A]]
  override def filterNot (pred :A => Boolean) :Ordered[A] =
    super.filterNot(pred).asInstanceOf[Ordered[A]]
  override def foldBuild[B] (op :(Unordered.Builder[B],A) => Unit) :Ordered[B] =
    super.foldBuild(op).asInstanceOf[Ordered[B]]
  override def foldBuildJ[B] (op :BiConsumer[_ >: Unordered.Builder[B],_ >: A]) :Ordered[B] =
    super.foldBuildJ(op).asInstanceOf[Ordered[B]]
  override def map[B] (f :A => B) :Ordered[B] = super.map(f).asInstanceOf[Ordered[B]]

  override def newBuilder[B] (expectedSize :Int = 4) :Builder[B]
  override def newEmpty[B] :Ordered[B]

  /** Internal slice implementation that does not check bounds. */
  protected def uncheckedSlice (from :Int, until :Int) :Ordered[A] = {
    val iter = Iterables.skip(iterator(), from)
    val bb = newBuilder[A](until-from)
    var ii = from ; while (ii < until) { bb += iter.next ; ii += 1 }
    bb.build()
  }

  /** Builds a new instance of the appropriate concrete collection type, initialized with the
    * supplied elements. */
  protected def build[B] (elems :Array[Any], size :Int) :Ordered[B]
}

object Ordered {

  /** An [[Unordered.Builder]] with return type specialized to [[Ordered]]. */
  trait Builder[E] extends Unordered.Builder[E] {
    // specialize the return type of build()
    override def build () :Ordered[E]
  }

  /** Returns an ordered view of `as`. `as` is assumed to be effectively immutable for the lifetime
    * of this view. Violate this assumption at your peril. */
  def view[A] (as :Collection[A]) :Ordered[A] = new Ordered[A]() {
    override def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) :Unit =
      Iterables.copyInto(as, start, end, target.asInstanceOf[Array[Object]], offset)
    override def size = as.size
    override def iterator () = as.iterator()
    override def newBuilder[B] (esize :Int) = Seq.builder[B](esize)
    override def newEmpty[B] = Seq.empty
    override protected def build[B] (elems :Array[Any], size :Int) = new Seq(elems, size)
    override protected def toStringType = "OrderedView"
  }
}
