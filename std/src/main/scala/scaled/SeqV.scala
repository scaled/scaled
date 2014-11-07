//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{AbstractList, Comparator, List => JList, Objects}

/** A view of an ordered sequence of elements which are O(1) accessible by index. The underlying
  * sequence may be mutable or immutable. Most of the time you should use the concrete class
  * [[Seq]] which is guaranteed to be immutable. Only in situations where you need to allow a
  * read-only view of a mutable Seq should you use this type directly.
  */
abstract class SeqV[+A] extends Ordered[A] {

  /** Returns the `index`th element of this seq.
    * @throws IndexOutOfBoundsException if the index is out of bounds. Natch.
    */
  def get (index :Int) :A

  /** Returns the `index`th element of this seq.
    * @throws IndexOutOfBoundsException if the index is out of bounds. Natch.
    */
  def apply (index :Int) :A = get(index)

  /** Returns true if `this` contains `slice` as a subsequence. */
  def containsSlice[B >: A] (slice :SeqV[B]) :Boolean = indexOfSlice(slice) != -1

  /** Returns true if this sequence ends with `slice`. */
  def endsWith[B >: A] (slice :SeqV[B]) :Boolean = sliceEquals(slice, size-slice.length)

  /** Folds `op` over the elements of this seq, from right to left. */
  def foldRight[B] (zero :B)(op :(A,B) => B) :B = {
    var acc = zero
    var ii = size-1 ; while (ii >= 0) { acc = op(get(ii), acc) ; ii -= 1 }
    acc
  }
  /** An alias for [[foldRight]]. */
  @inline final def :\[B] (zero :B)(op :(A,B) => B) :B = foldRight(zero)(op)

  /** Returns the index (>= `from`) of the first occurrence of `elem` (per [[Objects.equals]]) in
    * this seq, or -1 if `elem` is not found in the seq. */
  def indexOf[B >: A] (elem :B, from :Int = 0) :Int = {
    val ss = size
    var ii = from ; while (ii < ss) { if (Objects.equals(get(ii), elem)) return ii ; ii += 1 }
    -1
  }
  /** Returns the index of the first occurrence of `elem` (per [[Objects.equals]]) in this seq, or
    * -1 if `elem` is not found in the seq. */
  def indexOf[B >: A] (elem :B) :Int = indexOf(elem, 0)

  /** Returns the index (>= `from`) of the first element that satisfies `pred`, or -1. */
  def indexWhere (pred :A => Boolean, from :Int) :Int = {
    val ss = size ; var ii = from ; while (ii < ss) { if (pred(get(ii))) return ii ; ii += 1 }
    -1
  }
  /** Returns the index of the first element that satisfies `pred`, or -1. */
  def indexWhere (pred :A => Boolean) :Int = indexOf(pred, 0)

  /** Returns the index (>= `from`) of the first occurrence of `slice` in this sequence or -1. */
  def indexOfSlice[B >: A] (slice :SeqV[B], from :Int) :Int = {
    val ll = size-slice.size
    var ii = from ; while (ii < ll) { if (sliceEquals(slice, ii)) return ii ; ii += 1 }
    -1
  }
  /** Returns the index of the first occurrence of `slice` in this sequence or -1. */
  def indexOfSlice[B >: A] (slice :SeqV[B]) :Int = indexOfSlice(slice, 0)

  /** Returns the index (<= `from`) of the last occurrence of `elem` (per [[Objects.equals]]) in
    * this seq, or -1 if `elem` is not found in the seq. */
  def lastIndexOf[B >: A] (elem :B, from :Int) :Int = {
    var ii = from ; while (ii >= 0) { if (Objects.equals(get(ii), elem)) return ii ; ii -= 1 }
    -1
  }
  /** Returns the index of the last occurrence of `elem` (per [[Objects.equals]]) in this seq, or
    * -1 if `elem` is not found in the seq. */
  def lastIndexOf[B >: A] (elem :B) :Int = lastIndexOf(elem, size-1)

  /** Returns the index (<= `from`) of the last element that satisfies `pred`, or -1. */
  def lastIndexWhere (pred :A => Boolean, from :Int) :Int = {
    var ii = from ; while (ii >= 0) { if (pred(get(ii))) return ii ; ii -= 1 }
    -1
  }
  /** Returns the index of the last element that satisfies `pred`, or -1. */
  def lastIndexWhere (pred :A => Boolean) :Int = lastIndexOf(pred, size-1)

  /** Returns the index (<= `from`) of the last occurrence of `slice` in this sequence or -1. */
  def lastIndexOfSlice[B >: A] (slice :SeqV[B], from :Int) :Int = {
    var ii = from ; while (ii >= 0) { if (sliceEquals(slice, ii)) return ii ; ii -= 1 }
    -1
  }
  /** Returns the index of the last occurrence of `slice` in this sequence or -1. */
  def lastIndexOfSlice[B >: A] (slice :SeqV[B]) :Int = lastIndexOfSlice(slice, size-slice.size)

  /** Returns true if this collection contains `slice` starting at `index`. */
  def sliceEquals[B >: A] (slice :SeqV[B], index :Int) :Boolean = {
    val ll = index+slice.size
    if (index < 0 || ll > size) return false
    var ss = 0 ; var ii = index ; while (ii < ll) {
      if (!Objects.equals(get(ii), slice.get(ss))) return false
      ii += 1 ; ss += 1
    }
    true
  }

  /** Returns true if this sequence starts with `slice`. */
  def startsWith[B >: A] (slice :SeqV[B]) :Boolean = sliceEquals(slice, 0)

  /** Reduces this collection, from right to left, via `op`.
    * @throws NoSuchElementException when called on an empty collection. */
  def reduceRight[A1 >: A] (op :(A1,A1) => A1) :A1 = {
    var acc :A1 = last
    var ii = size-2 ; while (ii >= 0) { acc = op(get(ii), acc) ; ii -= 1 }
    acc
  }

  /** Returns this seq in reverse order. */
  def reverse :Seq[A] = reverseMap(identity)

  /** Returns this seq in reverse order, mapped via `f`. */
  def reverseMap[B] (f :A => B) :Seq[B] = {
    val sb = Seq.builder[B](size)
    var ii = size-1 ; while (ii >= 0) { sb += f(get(ii)) ; ii -= 1 }
    sb.build()
  }

  /** Retypes this seq of `A`s as a seq of `B`s. Makes life easier in Java. */
  def upcast[B >: A] :SeqV[B] = this

  /** Returns a copy of `this` with `b` prepended to it. This is wildly inefficient, **do not** use
    * this to build seqs from scratch. Use [[Seq.builder]]. */
  def +:[B >: A] (b :B) :Seq[B] = Seq.builder[B](size+1).append(b).append(this).build()

  /** Returns a copy of `this` with `b` appended to it. This is wildly inefficient, **do not** use
    * this to build seqs from scratch. Use [[Seq.builder]]. */
  def :+[B >: A] (b :B) :Seq[B] = Seq.builder[B](size+1).append(this).append(b).build()

  // views
  /** Returns a view of this seq as a [[JList]]. */
  def asJList[B >: A] :JList[B] = new AbstractList[B]() {
    override def size = SeqV.this.size
    override def get (idx :Int) = SeqV.this.get(idx)
  }

  // overrides for performance and type specificity
  override def collect[B] (pf :PartialFunction[A,B]) :Seq[B] = super.collect(pf).toSeq
  override def concat[B >: A] (that :Iterable[B]) :Seq[B] = super.concat(that).toSeq
  override def ++[B >: A] (bs :Iterable[B]) :Seq[B] = concat(bs)

  override def distinct () :Seq[A] = super.distinct().toSeq

  override def drop (count :Int) :Seq[A] = super.drop(count).toSeq
  override def dropRight (count :Int) :Seq[A] = super.dropRight(count).toSeq
  override def dropWhile (pred :A => Boolean) :Seq[A] = super.dropWhile(pred).toSeq

  override def filter (pred :A => Boolean) :Seq[A] = super.filter(pred).toSeq
  override def filterNot (pred :A => Boolean) :Seq[A] = super.filterNot(pred).toSeq

  override def flatMap[B] (f :A => Iterable[B]) :Seq[B] = super.flatMap(f).toSeq

  override def foldBuild[B] (op :(Unordered.Builder[B],A) => Unit) :Seq[B] = {
    val sb = Seq.builder[B]()
    var ii = 0 ; val ll = size ; while (ii < ll) { op(sb, get(ii)) ; ii += 1 }
    sb.build()
  }
  override def fold[B] (zero :B)(op :(B,A) => B) :B = {
    var acc = zero
    var ii = 0 ; val ll = size ; while (ii < ll) { acc = op(acc, get(ii)) ; ii += 1 }
    acc
  }

  override def grouped (size :Int) :Seq[Seq[A]] = super.grouped(size).asInstanceOf[Seq[Seq[A]]]
  override def groupBy[K] (f :A => K) :Map[K,Seq[A]] =
    super.groupBy(f).asInstanceOf[Map[K,Seq[A]]]

  override def head = get(0)
  override def last :A = size match {
    case 0 => throw new NoSuchElementException("'last' called on empty seq")
    case n => get(n-1)
  }

  override def partition (pred :A => Boolean) :(Seq[A],Seq[A]) =
    super.partition(pred).asInstanceOf[(Seq[A],Seq[A])]

  override def prefixLength (pred :A => Boolean) = {
    var ii = 0 ; var ll = size ; while (ii < ll && pred(get(ii))) { ii += 1 }
    ii
  }

  override def map[B] (f :A => B) :Seq[B] = super.map(f).toSeq
  override def scan[B] (zero :B)(op :(B,A) => B) :Seq[B] = super.scan(zero)(op).toSeq
  override def sorted (cmp :Comparator[_ >: A]) :Seq[A] = super.sorted(cmp).toSeq
  override def sorted (implicit cmp :Ordering[_ >: A]) :Seq[A] = super.sorted(cmp).toSeq
  override def sortBy[B] (fn :(A => B), cmp :Comparator[B]) :Seq[A] = super.sortBy(fn, cmp).toSeq
  override def sortBy[B] (fn :(A => B))(implicit cmp :Ordering[B]) :Seq[A] =
    super.sortBy(fn)(cmp).toSeq

  override def slice (from :Int, until :Int) :Seq[A] = super.slice(from, until).toSeq

  override def zip[B] (that :Ordered[B]) :Seq[(A,B)] = super.zip(that).toSeq
  override def zipAll[A1 >: A,B] (that :Ordered[B], a :A1, b :B) :Seq[(A1,B)] =
    super.zipAll(that, a, b).toSeq
  override def zipWithIndex :Seq[(A,Int)] = super.zipWithIndex.toSeq

  override def newBuilder[B] (expectedSize :Int) = new Seq.Builder[B](expectedSize)
  override def newEmpty[B] = Seq.empty

  override def equals (other :Any) :Boolean = other match {
    case oseq  :SeqV[_]      => Seq.equals(this, oseq)
    case oiter :JIterable[_] => Iterables.equals(this, oiter)
    case _ => false
  }

  override protected def uncheckedSlice (from :Int, until :Int) :Ordered[A] = {
    val elems = new Array[Any](until-from)
    copyInto(from, until, elems, 0)
    new Seq[A](elems, elems.length)
  }

  override protected def build[B] (elems :Array[Any], size :Int) = new Seq(elems, size)
  override protected def toStringType = "Seq"
}
