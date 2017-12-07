//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.function.BiConsumer
import java.util.{Collection, Comparator, HashMap}

/** An unordered collection of unknown but finite length. This makes available a variety of
  * length-agnostic operations on Java iterables (via an implicit conversion).
  *
  * Note that when a Java iterable is promoted to a Scaled iterable, its concrete type is [[Seq]].
  *
  * @define ORDER The result will be ordered if the underlying collection is ordered, and unordered
  * otherwise.
  */
abstract class Iterable[+A] extends JIterable[A @uV] {
  import Unordered.Builder

  /** Returns a collection containing the elements of this collection for which `pf` is defined.
    * The elements of the new collection will have been mapped by `pf`. $ORDER */
  def collect[B] (pf :PartialFunction[A,B]) :Unordered[B] = foldBuild[B] { (b,a) =>
    val res = pf.applyOrElse(a, Unordered.PartialNotApplied)
    if (res.asInstanceOf[AnyRef] ne Unordered.PartialNotApplied) b += res.asInstanceOf[B]
  }

  /** Returns a collection containing `this` and `that`. If the underlying collection is
    * [[Ordered]] it will contain elements from `this` followed by elements from `that`. If it is
    * not ordered, then the order of the elements in the new collection is undefined. */
  def concat[B >: A] (that :Iterable[B]) :Unordered[B] = {
    val sb = newBuilder[B](sizeHint + that.sizeHint)
    sb ++= this
    sb ++= that
    sb.build()
  }
  /** Returns a collection containing `this` and `that`. This is for Java interop. Scala code
    * should use [[concat(Iterable)]] as it does the right thing with regard to variance. */
  def concat[B >: A] (that :JIterable[B]) :Unordered[B] = concat(that :Iterable[B])
  /** Returns a collection containing `this` and then `that`. Synonym for [[concat]]. */
  def ++[B >: A] (bs :Iterable[B]) :Unordered[B] = concat(bs)

  /** Copies all elements of this collection into `target` at `offset` in iteration order.
    * @throws ArrayIndexOutOfBoundsException if target does not contain enough space at offset to
    * hold the elements.
    */
  def copyInto (target :Array[Any], offset :Int) {
    var ii = offset ; val iter = iterator() ; while (iter.hasNext) {
      target(ii) = iter.next
      ii += 1
    }
  }

  /** Returns a count of elements that match `pred`. */
  def count (pred :A => Boolean) :Int = {
    var count = 0
    val iter = iterator() ; while (iter.hasNext) if (pred(iter.next)) count += 1
    count
  }

  /** Returns true if this collection contains at least one element which satisfies `pred`. */
  def exists (pred :A => Boolean) :Boolean = {
    val iter = iterator() ; while (iter.hasNext) if (pred(iter.next)) return true
    false
  }

  /** Returns the first element (by iteration order) that satisfies `pred` or `None`. */
  def find (pred :A => Boolean) :Option[A] = {
    val iter = iterator() ; while (iter.hasNext) {
      val a = iter.next
      if (pred(a)) return Some(a)
    }
    None
  }

  /** Partitions this collection into a map of collections based on discriminator function `f`.
    * $ORDER */
  def groupBy[K] (f :A => K) :Map[K,Unordered[A]] = groupBy(f, a => a)

  /** Partitions this collection into a map of collections based on key discriminator function
    * `kf` and value discriminator function `vf`.
    * $ORDER */
  def groupBy[K,V] (kf :A => K, vf :A => V) :Map[K,Unordered[V]] = {
    val bs = new HashMap[K,Builder[V]]()
    val iter = iterator() ; while (iter.hasNext) {
      val a = iter.next
      Mutable.getOrPut(bs, kf(a), newBuilder[V]()) += vf(a)
    }
    val mb = Map.builder[K,Unordered[V]]()
    val meiter = bs.entrySet.iterator() ; while (meiter.hasNext) {
      val me = meiter.next
      mb += (me.getKey, me.getValue.build())
    }
    mb.build()
  }

  /** Folds `op` over this collection, in iteration order. */
  def fold[B] (zero :B)(op :(B,A) => B) :B = {
    var acc = zero ; val iter = iterator()
    while (iter.hasNext) acc = op(acc, iter.next)
    acc
  }

  /** Applies `f` to each element of this list and returns a collection that contains the
    * "concatenation" of the results. */
  def flatMap[B] (f :A => JIterable[B]) :Iterable[B] = foldBuild[B]((b, a) => b ++= f(a))

  /** Applies `op` to each element of this collection, in iteration order. */
  def foreach[U] (op :A => U) :Unit = {
    val iter = iterator() ; while (iter.hasNext) op(iter.next)
  }

  /** Returns true if `op` returns true for all elements in this collection. */
  def forall[B >: A] (op :B => Boolean) :Boolean = {
    val iter = iterator() ; while (iter.hasNext) if (!op(iter.next)) return false
    true
  }

  /** Returns all elements of this collection which satisfy `pred`. $ORDER */
  def filter (pred :A => Boolean) :Unordered[A] = foldBuild[A]((b, a) => if (pred(a)) b += a)
  /** Returns all elements of this collection which do not satisfy `pred`. $ORDER */
  def filterNot (pred :A => Boolean) :Unordered[A] = foldBuild[A]((b, a) => if (!pred(a)) b += a)

  /** Folds `op` over this collection, building a new collection in the process. The collection
    * will have the same underlying concrete type and ordering guarantees as this collection. */
  def foldBuild[B] (op :(Builder[B],A) => Unit) :Unordered[B] = {
    val bb = newBuilder[B]()
    val iter = iterator ; while (iter.hasNext) op(bb, iter.next)
    bb.build()
  }

  /** Folds `op` over this collection, building a new collection in the process. The collection
    * will have the same underlying concrete type and ordering guarantees as this collection. */
  def foldBuildJ[B] (op :BiConsumer[_ >: Builder[B],_ >: A]) :Unordered[B] = {
    val bb = newBuilder[B]()
    val iter = iterator ; while (iter.hasNext) op.accept(bb, iter.next)
    bb.build()
  }

  /** Returns a new seq which contains `f` applied to each of this collection's elements. $ORDER */
  def map[B] (f :A => B) :Unordered[B] = {
    // we don't use foldBuild because we know we're building a collection the same size as this
    // one, so we want to hint that to the builder
    val bb = newBuilder[B](sizeHint)
    val iter = iterator ; while (iter.hasNext) bb += f(iter.next)
    bb.build()
  }

  /** Converts this collection into a map by applying a key function `f` to each element. In the
    * case where more than one element maps to the same key, only one element will be chosen and
    * which element is not defined. */
  def mapBy[K] (f :A => K) :Map[K,A] = fold(Map.builder[K,A]())((b, e) => b.put(f(e), e)).build()

  /** Returns the largest element of this collection, based on `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def max (cmp :Comparator[_ >: A]) :A =
    reduce((m, n) => if (cmp.compare(n, m) > 0) n else m)
  /** Returns the largest element of this collection, based on `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def max (implicit cmp :Ordering[_ >: A]) :A = max(cmp :Comparator[_ >: A])

  /** Returns the largest element of this collection, mapped via `fn` and compared with `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def maxBy[B] (fn :(A => B), cmp :Comparator[B]) :A = {
    val iter = iterator() ; var max = iter.next ; var fmax = fn(max)
    while (iter.hasNext) {
      val next = iter.next ; val fnext = fn(next)
      if (cmp.compare(fnext, fmax) > 0) { max = next ; fmax = fnext }
    }
    max
  }
  /** Returns the largest element of this collection, mapped via `fn` and compared with `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def maxBy[B] (fn :(A => B))(implicit cmp :Ordering[B]) :A = maxBy(fn, cmp :Comparator[B])

  /** Returns the smallest element of this collection, based on `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def min (cmp :Comparator[_ >: A]) :A =
    reduce((m, n) => if (cmp.compare(n, m) < 0) n else m)
  /** Returns the largest element of this collection, based on `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def min (implicit cmp :Ordering[_ >: A]) :A = min(cmp :Comparator[_ >: A])

  /** Returns the largest element of this collection, mapped via `fn` and compared with `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def minBy[B] (fn :(A => B), cmp :Comparator[B]) :A = {
    val iter = iterator() ; var min = iter.next ; var fmin = fn(min)
    while (iter.hasNext) {
      val next = iter.next ; val fnext = fn(next)
      if (cmp.compare(fnext, fmin) < 0) { min = next ; fmin = fnext }
    }
    min
  }
  /** Returns the largest element of this collection, mapped via `fn` and compared with `cmp`.
    * @throws NoSuchElementException when called on an empty collection. */
  def minBy[B] (fn :(A => B))(implicit cmp :Ordering[B]) :A = minBy(fn, cmp :Comparator[B])

  // these hose type inference, so we omit them and put them in Unordereds instead
  // def maxBy[B <: Comparable[B]] (fn :(A => B)) :A = maxBy(fn, Comparator.naturalOrder[B])
  // def minBy[B <: Comparable[B]] (fn :(A => B)) :A = minBy(fn, Comparator.naturalOrder[B])

  /** Partitions this collection into two collections: one consisting of elements that satisfy
    * `pred` and the other of those which do not. $ORDER */
  def partition (pred :A => Boolean) :(Unordered[A],Unordered[A]) = {
    val ys = newBuilder[A]() ; val ns = newBuilder[A]()
    this foreach { e => if (pred(e)) ys += e else ns += e }
    (ys.build(), ns.build())
  }

  /** Reduces this collection, in iteration order, via `op`.
    * @throws NoSuchElementException when called on an empty collection. */
  def reduce[A1 >: A] (op :(A1,A1) => A1) :A1 = {
    val iter = iterator() ; var acc :A1 = iter.next
    while (iter.hasNext) acc = op(acc, iter.next)
    acc
  }

  /** Returns a lazily computed filtered view of this collection.
    * This chiefly exists to support Scala's for comprehension syntax. */
  def withFilter (pred :(A => Boolean)) :Filtered[A] = {
    class FilteredIterable(source :Iterable[A], pred :(A => Boolean)) extends Filtered[A] {
      def map[B] (fn :(A => B)) = source.filter(pred).map(fn)
      def flatMap[B] (fn :(A => JIterable[B])) = source.filter(pred).flatMap(fn)
      def foreach[U] (fn :(A => U)) = source.filter(pred).foreach(fn)
      def withFilter (pred :(A => Boolean)) =
        new FilteredIterable(source, a => this.pred(a) && pred(a))
    }
    new FilteredIterable(this, pred)
  }

  /** Converts this collection into an (immutable) [[Seq]].
    * If the collection is already an immutable [[Seq]], `this` is returned. */
  def toSeq :Seq[A] = Seq.builder[A](sizeHint).append(this).build()

  /** Converts this collection into a [[List]].
    * If the collection is already a [[List]], `this` is returned. */
  def toList :List[A] = List.builder[A]().append(this).build()

  /** Converts this collection into a [[Set]].
    * If the collection is already a [[Set]], `this` is returned. */
  def toSet[B >: A] :Set[B] = Set.builder[B](sizeHint).append(this :Iterable[B]).build()

  /** Converts this collection into a [[Map]]. The collection must consist of `(key, value)` pairs
    * (`scala.Tuple2`). The implicit `ev` argument is injected by the Scala compiler when it can
    * prove this. Callers from other languages may supply a typed null expression as the value of
    * the evidence is not used at runtime. */
  def toMap[K,V] (implicit ev: <:<[A,(K,V)]) :Map[K,V] =
    Map.builder[K,V](sizeHint).append(this.asInstanceOf[Iterable[(K,V)]]).build()

  /** Returns the size of this collection, if available. This hint is used to prepare builders when
    * transforming this collection. */
  def sizeHint :Int

  /** Returns a builder that builds the appropriate concrete collection type. */
  def newBuilder[B] (expectedSize :Int = 4) :Unordered.Builder[B]
  /** Returns the empty instance of the appropriate concrete collection type. */
  def newEmpty[B] :Unordered[B]

  /** Converts this collection to a string, with empty separator and open/close brackets. */
  def mkString :String = mkString("")
  /** Converts this collection to a string, with `sep` separator and empty open/close brackets. */
  def mkString (sep :String) :String = mkString("", sep, "")
  /** Converts this collection to a string, with the specified open/close brackets and separator. */
  def mkString (open :String, sep :String, close :String) :String =
    addString(new JStringBuilder(), open, sep, close).toString

  /** Appends this collection to `sb`, first appending `open`, then appending the collection
    * elements, separated by `sep` and finally appending `close`.
    * @return `sb` for convenient call chaining. */
  def addString (sb :JStringBuilder, open :String, sep :String, close :String) :JStringBuilder = {
    sb.append(open)
    val ll = sb.length
    fold(sb) { (sb, e) =>
      if (sb.length > ll) sb.append(sep)
      sb.append(e)
    }
    sb.append(close)
  }

  // views
  /** Returns a view of this iterable as a Scala [[SIterable]]. */
  def asSIterable :SIterable[A] = new SIterable[A]() {
    override def iterator = new SIterator[A]() {
      private[this] val iter = Iterable.this.iterator()
      def next = iter.next
      def hasNext = iter.hasNext
    }
  }

  override def hashCode () = {
    var code = 1
    val iter = iterator() ; while (iter.hasNext) {
      val elem = iter.next
      code = 31 * code + (if (elem == null) 0 else elem.hashCode)
    }
    code
  }

  override def toString () :String =
    addString(new JStringBuilder(toStringType), "(", ", ", ")").toString

  /** Returns the type name used when generating [[toString]] result. */
  protected def toStringType :String
}

object Iterable {

  /** Returns a view of the Java iterable `iable` as a Scaled iterable. */
  def view[A] (iable :JIterable[A]) :Iterable[A] = new Iterable[A] {
    override def iterator () = iable.iterator()
    override def sizeHint = iable match {
      case c :Collection[_] => c.size
      case _ => 0
    }
    override def newBuilder[B] (esize :Int) = Seq.builder(esize)
    override def newEmpty[B] = Seq.empty
    override protected def toStringType = "IterableView"
  }

  /** Returns a view of the Scala iterable `iable` as a Scaled iterable. */
  def view[A] (iable :scala.Iterable[A]) :Iterable[A] = new Iterable[A] {
    override def iterator () = new JIterator[A @uV]() {
      private[this] val iter = iable.iterator
      def hasNext = iter.hasNext
      def next = iter.next
    }
    override def sizeHint = if (iable.hasDefiniteSize) iable.size else 0
    override def newBuilder[B] (esize :Int) = Seq.builder[B](esize)
    override def newEmpty[B] = Seq.empty
    override protected def toStringType = "IterableView"
  }
}
