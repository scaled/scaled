//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{Collection, Comparator, List => JList, NoSuchElementException, Objects}
import scala.{collection => sc}

/** A simple functional list class, built from cons cells as God and John McCarthy intended. */
sealed abstract class List[+A] extends Ordered[A] {

  /** Returns the tail of this list.
    * @throws NoSuchElementException if called on the `nil` list. */
  def tail :List[A]

  /** Returns a new list with `elem` consed onto its head. */
  def cons[B >: A] (elem :B) :List[B]

  /** Returns a new list with `elem` consed onto its head. */
  def ::[B >: A] (elem :B) :List[B] = cons(elem)

  /** @inheritDoc Note: this operation is O(N), not O(1). */
  def size :Int

  /** Folds `op` over the elements of this list, from right to left.
    * NOTE: this uses recursion and should thus not be applied to extremely long lists. */
  def foldRight[B] (zero :B)(op :(A,B) => B) :B
  /** An alias for [[foldRight]]. */
  @inline final def :\[B] (zero :B)(op :(A,B) => B) :B = foldRight(zero)(op)

  /** Returns this list in reverse order. */
  def reverse :List[A] = reverseMap(identity)

  /** Returns this list in reverse order, mapped via `f`. */
  def reverseMap[B] (f :A => B) :List[B] = {
    var rev :List[B] = Nil ; var ll = this
    while (!ll.isEmpty) { rev = f(ll.head) :: rev ; ll = ll.tail }
    rev
  }

  /** Retypes this list of `A`s as a list of `B`s. Makes life easier in Java. */
  def upcast[B >: A] :List[B] = this

  /** Returns true if this list is equal to `other`.
    * This is used to implement [[equals]] efficiently. */
  def equiv (other :List[_]) :Boolean
  /** Combines the hash code of this list with `code`.
    * This is used to implement [[hashCode]] efficiently. */
  def hashCode (code :Int) :Int

  override def equals (other :Any) :Boolean = other match {
    case olist :List[_] => equiv(olist)
    case oiter :JIterable[_] => Iterables.equals(this, oiter)
    case _ => false
  }
  override def hashCode :Int = hashCode(1)

  // overrides for performance and type specificity
  override def collect[B] (pf :PartialFunction[A,B]) :List[B] = super.collect(pf).toList
  override def concat[B >: A] (that :Iterable[B]) :List[B] = super.concat(that).toList
  override def ++[B >: A] (bs :Iterable[B]) :List[B] = concat(bs)

  override def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) :Unit = {
    if (start > 0) drop(start).copyInto(0, end-start, target, offset)
    else {
      var ll = this ; var ii = 0 ; while (ii < end) {
        target(offset+ii) = ll.head
        ii += 1
        ll = ll.tail
      }
    }
  }

  override def distinct () :List[A] = super.distinct().toList

  override def drop (count :Int) :List[A] = {
    var ll = this ; var ii = 0
    while (ii < count && !ll.isEmpty) { ll = ll.tail ; ii += 1 }
    ll
  }
  override def dropRight (count :Int) :List[A] = {
    if (count <= 0) this
    else {
      var counter = drop(count)
      if (counter.isEmpty) Nil
      else {
        val lb = List.builder[A]()
        var list = this ; while (!counter.isEmpty) {
          lb += list.head
          list = list.tail
          counter = counter.tail
        }
        lb.build()
      }
    }
  }
  override def dropWhile (pred :A => Boolean) :List[A] = {
    var ll = this ; var ii = 0 ; while (!ll.isEmpty && pred(ll.head)) { ll = ll.tail ; ii += 1 }
    ll
  }

  override def endsWith[B >: A] (suffix :Ordered[B]) :Boolean = {
    val size = this.size ; val ssize = suffix.size
    (ssize <= size) && (drop(size-ssize) == suffix)
  }

  override def filter (pred :A => Boolean) :List[A] = super.filter(pred).toList
  override def filterNot (pred :A => Boolean) :List[A] = super.filterNot(pred).toList

  override def flatMap[B] (f :A => JIterable[B]) :List[B] = super.flatMap(f).toList

  override def foldBuild[B] (op :(Unordered.Builder[B],A) => Unit) :List[B] = {
    val lb = List.builder[B]() ; var ll = this
    while (!ll.isEmpty) { op(lb, ll.head) ; ll = ll.tail }
    lb.build()
  }
  override def fold[B] (zero :B)(op :(B,A) => B) :B = {
    var acc = zero ; var ll = this
    while (!ll.isEmpty) { acc = op(acc, ll.head) ; ll = ll.tail }
    acc
  }

  override def grouped (size :Int) :List[List[A]] = super.grouped(size).asInstanceOf[List[List[A]]]
  override def groupBy[K] (f :A => K) :Map[K,List[A]] =
    super.groupBy(f).asInstanceOf[Map[K,List[A]]]

  override def last :A = {
    if (isEmpty) throw new NoSuchElementException("'last' called on empty list")
    var ll = this ; while (!ll.tail.isEmpty) ll = ll.tail
    ll.head
  }

  override def partition (pred :A => Boolean) :(List[A],List[A]) =
    super.partition(pred).asInstanceOf[(List[A],List[A])]

  override def prefixLength (pred :A => Boolean) = {
    var ii = 0 ; var ll = this ; while (!ll.isEmpty && pred(ll.head)) { ii += 1 ; ll = ll.tail }
    ii
  }

  override def map[B] (f :A => B) :List[B] = super.map(f).toList
  override def scan[B] (zero :B)(op :(B,A) => B) :List[B] = super.scan(zero)(op).toList
  override def sorted (cmp :Comparator[_ >: A]) :List[A] = super.sorted(cmp).toList
  override def sorted (implicit cmp :Ordering[_ >: A]) :List[A] = super.sorted(cmp).toList
  override def sortBy[B] (fn :(A => B), cmp :Comparator[B]) :List[A] = super.sortBy(fn, cmp).toList
  override def sortBy[B] (fn :(A => B))(implicit cmp :Ordering[B]) :List[A] =
    super.sortBy(fn)(cmp).toList

  override def take (count :Int) :List[A] = super.take(count).toList
  override def takeRight (count :Int) :List[A] = super.takeRight(count).toList
  override def takeWhile (pred :A => Boolean) :List[A] = super.takeWhile(pred).toList

  override def zip[B] (that :Ordered[B]) :List[(A,B)] = super.zip(that).toList
  override def zipAll[A1 >: A,B] (that :Ordered[B], a :A1, b :B) :List[(A1,B)] =
    super.zipAll(that, a, b).toList
  override def zipWithIndex :List[(A,Int)] = super.zipWithIndex.toList

  override def toList = this

  override def newBuilder[B] (expectedSize :Int) = new List.Builder[B]()
  override def newEmpty[B] = List.nil

  override protected def uncheckedSlice (from :Int, until :Int) :Ordered[A] = {
    var tip = this ; var ii = 0 ; while (ii < from) { tip = tip.tail ; ii += 1}
    val length = until - from
    if (tip.length == length) tip else {
      val bb = newBuilder[A](length)
      ii = 0 ; while (ii < length) { bb += tip.head ; tip = tip.tail ; ii += 1 }
      bb.build()
    }
  }

  override protected def build[B] (elems :Array[Any], size :Int) = List.build(elems, size)
  override protected def toStringType = "List"
}

/** "Static" list stuffs. */
object List {

  /** Used to build [[List]]s. */
  class Builder[A] extends Ordered.Builder[A] {
    private var _head :List[A] = null
    private var _last :Cons[A] = null

    override def append (value :A) = {
      if (_head == null) {
        _last = new Cons(value, null)
        _head = _last
      } else {
        val tail = new Cons(value, null)
        _last.unsafeSetTail(tail)
        _last = tail
      }
      this
    }
    override def append (iter :JIterator[_ <: A]) = {
      while (iter.hasNext) append(iter.next)
      this
    }
    override def append (elems :JIterable[_ <: A]) = append(elems.iterator())
    override def append (elems :Unordered[A]) = append(elems.iterator())
    override def append (elems :sc.Traversable[A]) = { elems foreach append ; this }

    override def += (value :A) = append(value)
    override def ++= (iter :JIterator[_ <: A]) = append(iter)
    override def ++= (elems :JIterable[_ <: A]) = append(elems)
    override def ++= (elems :Unordered[A]) = append(elems)
    override def ++= (elems :sc.Traversable[A]) = append(elems)

    override def build () :List[A] = if (_head == null) Nil else {
      _last.unsafeSetTail(Nil)
      _last = null
      val list = _head
      _head = null
      list
    }
  }

  /** Returns a [[List]] builder. */
  def builder[A] () :Builder[A] = new Builder()

  /** Returns the empty list. */
  def nil[A] :List[A] = Nil.asInstanceOf[List[A]]
  /** Returns the empty list. */
  def apply[A] () :List[A] = nil
  /** Creates a list that contains `value`. */
  def apply[A] (value :A) :List[A] = value :: nil
  /** Creates a list that contains `[a0, a1]`. */
  def apply[A] (a0 :A, a1 :A) :List[A] = a0 :: a1 :: nil
  /** Creates a list that contains `[a0, a1, a2]`. */
  def apply[A] (a0 :A, a1 :A, a2 :A) :List[A] = a0 :: a1 :: a2 :: nil
  /** Creates a list that contains `[a0, a1, a2]`. */
  def apply[A] (a0 :A, a1 :A, a2 :A, a3 :A) :List[A] = a0 :: a1 :: a2 :: a3 :: nil
  /** Creates a list that contains `values`. */
  def apply[A] (a0 :A, a1 :A, a2 :A, a3 :A, aN :A*) :List[A] = a0 :: a1 :: a2 :: a3 :: copyOf(aN)

  /** Creates a list that contains `values`. */
  def from[A] (values :Array[A]) :List[A] = {
    var list = nil[A]
    var ii = values.length-1 ; while (ii >= 0) {
      list = values(ii) :: list
      ii -= 1
    }
    list
  }

  /** Returns a set containing the elements of `as`. */
  def copyOf[A] (as :Collection[A]) :List[A] =
    if (as.isEmpty) Nil else new Builder[A]().append(as).build()

  /** Returns a set containing the elements of `as`. */
  def copyOf[A] (as :JList[A]) :List[A] = {
    var list = nil[A]
    var ii = as.size-1 ; while (ii >= 0) { list = as.get(ii) :: list ; ii -= 1 }
    list
  }

  /** Creates a list that contains `values`. */
  def copyOf[A] (values :SSeq[A]) :List[A] = {
    var list = nil[A]
    var ii = values.length-1 ; while (ii >= 0) { list = values(ii) :: list ; ii -= 1 }
    list
  }

  private def build[A] (elems :Array[Any], size :Int) :List[A] = {
    var list = nil[A] ; var ii = size-1 ; while (ii >= 0) {
      list = (elems(ii).asInstanceOf[A] :: list)
      ii -= 1
    }
    list
  }
}

/** A cons cell: an element (`head`) and a pointer to the rest of the list (`tail`). */
private final class Cons[A] (override val head :A, private[this] var _tail :List[A])
    extends List[A] {

  /** DON'T CALL THIS. It's an internal mechanism to set the tail of this list, used by
    * [[List.Builder]] to enable lists to be built efficiently. It's public because there's no way
    * for us to make this private that will be enforced by any language other than Scala, so I'd
    * rather just make it public and document it. */
  def unsafeSetTail (tail :List[A]) :Unit = {
    _tail = tail
  }

  override def iterator () = new JIterator[A]() {
    private var _cur :List[A] = Cons.this
    override def hasNext = _cur ne Nil
    override def next = { val cur = _cur ; _cur = cur.tail ; cur.head }
  }

  override def isEmpty = false
  override def size = {
    var size = 0 ; var ll :List[A] = this
    while (!ll.isEmpty) { size += 1 ; ll = ll.tail }
    size
  }
  override def lengthCompare (n :Int) :Int = {
    var nn = n ; var ll :List[A] = this
    while (nn > 0) {
      if (ll.isEmpty) return -1
      nn -= 1
      ll = ll.tail
    }
    if (ll.isEmpty) 0 else 1
  }

  override def cons[B >: A] (elem :B) = new Cons(elem, this)
  override def headOption = Some(head)
  override def tail = _tail
  override def foldRight[B] (zero :B)(op :(A,B) => B) = op(head, _tail.foldRight(zero)(op))

  override def equiv (other :List[_]) = (this eq other) || (
    !other.isEmpty && Objects.equals(head, other.head) && _tail.equiv(other.tail))

  override def hashCode (code :Int) =
    _tail.hashCode(31 * code + (if (head == null) 0 else head.hashCode))
}

/** The empty `List`. */
case object Nil extends List[Nothing] {
  override def iterator () = Iterables.EMPTY_ITER.asInstanceOf[JIterator[Nothing]]
  override def isEmpty = true
  override def size = 0
  override def cons[B >: Nothing] (elem :B) :List[B] = new Cons(elem, this)
  override def head = throw new NoSuchElementException("Nil.head()")
  override def headOption = None
  override def tail = throw new NoSuchElementException("Nil.tail()")
  override def foldRight[B] (zero :B)(op :(Nothing,B) => B) = zero
  override def equiv (other :List[_]) = (this eq other)
  override def hashCode (code :Int) = code
}

/** Handles idiomatic pattern matching on list (i.e. `match h :: t`)). */
object :: {
  /** Unapplies a cons, for use in pattern matching. */
  def unapply[A] (list :List[A]) :Option[(A,List[A])] =
    if (list.isEmpty) None else Some((list.head, list.tail))
}
