//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{Collection, List => JList, Objects}

/** An immutable ordered sequence of elements. */
class Seq[+A] private[scaled] (_elems :Array[Any], _size :Int) extends SeqV[A] {

  /** Returns a [[scala.IndexedSeq]] with the contents of this seq. */
  def toScala :IndexedSeq[A] = {
    val full = scala.collection.mutable.WrappedArray.make[A](_elems)
    if (_elems.length == _size) full else full.slice(0, _size)
  }

  override def size :Int = _size
  override def get (index :Int) :A = _elems(index).asInstanceOf[A]
  override def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) {
    Seq.checkBounds(start, end, _size)
    System.arraycopy(_elems, start, target, offset, end-start)
  }
  override def toSeq :Seq[A] = this

  // specialize the return type here, for ergonomic reasons
  override def upcast[B >: A] :Seq[B] = this

  override def hashCode () = Std.hashCode(_elems.asInstanceOf[Array[Object]], _size)

  override def iterator () :JIterator[A @uV] = new JIterator[A]() {
    private var _index = 0
    override def hasNext :Boolean = _index < size
    override def next :A = {
      if (_index < size) try { get(_index) } finally { _index += 1 }
      else throw new NoSuchElementException()
    }
  }
}

object Seq {

  /** Used to build [[Seq]]s. */
  class Builder[A] (esize :Int) extends SeqBuffer[A](esize) with Ordered.Builder[A] {
    override def build () :Seq[A] = new Seq(elemsForBuild, size)
  }

  /** Returns a [[Seq]] builder. */
  def builder[A] () :Builder[A] = new Builder(4)
  /** Returns a [[Seq]] builder which is prepared to build a seq with at least `expectedSize`
    * elements. */
  def builder[A] (expectedSize :Int) :Builder[A] = new Builder(expectedSize)

  /** Returns the empty [[Seq]]. */
  def empty[A] :Seq[A] = EMPTY.asInstanceOf[Seq[A]]

  /** Returns a [[Seq]] containing `elems`. */
  def apply[A] (elems :A*) :Seq[A] = if (elems.isEmpty) empty else {
    val buffer = new Array[Any](elems.size)
    elems.copyToArray(buffer)
    new Seq(buffer, buffer.length)
  }

  /** Unapplies a seq (or view). */
  def unapplySeq[A] (seq :SeqV[A]) :Option[SeqV[A]] = Some(seq)

  /** Returns a [[Seq]] containing `elems`. */
  def copyOf[A] (elems :Collection[A]) :Seq[A] =
    new Seq[A](elems.toArray.asInstanceOf[Array[Any]], elems.size)

  // we box primitive arrays up front, rather than leaving them as is and unboxing every time an
  // element is accessed (which is how stock Scala collections do things)
  import BoxUtil._

  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Boolean]) :Seq[Boolean] = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Byte])    :Seq[Byte]    = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Char])    :Seq[Char]    = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Short])   :Seq[Short]   = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Int])     :Seq[Int]     = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Long])    :Seq[Long]    = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Float])   :Seq[Float]   = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from (elems :Array[Double])  :Seq[Double]  = new Seq(box(elems), elems.length)
  /** Returns a [[Seq]] containing `elems`. Ownership of `elems` is taken by the created seq. */
  def from[A <: AnyRef] (elems :Array[A]) :Seq[A] = new Seq(
    elems.asInstanceOf[Array[Any]], elems.length)

  /** Returns a seq view of `as`. `as` is assumed to be effectively immutable for the lifetime of
    * this view. Violate this assumption at your peril. */
  def view[A] (as :JList[A]) :SeqV[A] = new SeqV[A]() {
    override def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) {
      Seq.checkBounds(start, end, size)
      if (offset == 0) as.subList(start, end).toArray(target.asInstanceOf[Array[Object]])
      else Iterables.copyInto(as, start, end, target.asInstanceOf[Array[Object]], offset)
    }
    override def size = as.size
    override def iterator () = as.iterator()
    override def get (idx :Int) = as.get(idx)
    override protected def toStringType = "SeqView"
  }

  /** Returns true if `a1s` and `a2s` are the same size and their elements are pairwise equal, per
    * [[Objects.equals]]. */
  def equals (a1s :SeqV[_], a2s :SeqV[_]) :Boolean = {
    val size = a1s.size
    if (size != a2s.size) false
    else {
      var ii = 0 ; while (ii < size) {
        if (!Objects.equals(a1s.get(ii), a2s.get(ii))) return false
        ii += 1
      }
      true
    }
  }

  /** Checks that `idx` is in `[0,size)`.
    * @throws IndexOutOfBoundsException if it is not. */
  def checkIndex (idx :Int, size :Int) {
    if (idx < 0 || idx >= size) throw new IndexOutOfBoundsException(s"$idx not in [0,$size)")
  }

  /** Checks that `[from,until)` is a valid slice of `[0,size)`.
    * @throws IndexOutOfBoundsException if it is not. */
  def checkBounds (from :Int, until :Int, size :Int) {
    if (from < 0 || until < from || until > size)
      throw new IndexOutOfBoundsException(s"[$from,$until) not in [0,$size)")
  }

  private val EMPTY = new Seq(new Array[Any](0), 0)
}
