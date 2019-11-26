//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{Arrays, Collection, Objects}

/** A mutable ordered sequence of elements. */
class SeqBuffer[A] (initElems :Array[Any], initSize :Int) extends SeqV[A] with Cloneable {
  if (initElems.length == 0) throw new IllegalArgumentException(
    "SeqBuffer must have initial capacity of at least one element.")

  private[this] var _elems = initElems
  private[this] var _size = initSize

  private def expectAt (index :Int, count :Int, size :Int) :Array[Any] = {
    val els = _elems ; val remain = size-index ; val need = size + count
    if (need < els.length) {
      if (remain > 0) {
        System.arraycopy(els, index, els, index+count, remain) // shift [index,size)
        didExpand(index, count, els.length)
      }
      els
    } else {
      var ncap = els.length*2 ; while (ncap < need) ncap *= 2
      val nelems = new Array[Any](ncap)
      System.arraycopy(els, 0, nelems, 0, index) // copy [0,index)
      if (remain > 0) System.arraycopy(els, index, nelems, index+count, remain) // copy [index,size)
      _elems = nelems
      didExpand(index, count, ncap)
      nelems
    }
  }
  private def expect (size :Int, count :Int) = expectAt(size, count, size)

  /** Called when we have expanded our internal array. Used by builders for nefarious purposes. */
  protected def didExpand (index :Int, count :Int, ncap :Int) :Unit = {}

  /** Creates a buffer with the specified initial capacity (which must be >= 2). */
  def this (initCapacity :Int) = this(new Array[Any](math.max(2, initCapacity)), 0)

  /** Clears out the contents of this buffer, resetting its size to zero. Its internal array is
    * filled with nulls to ensure that spurious references are not retained. */
  def clear () :Unit = {
    Arrays.fill(_elems.asInstanceOf[Array[Object]], 0, _size, null)
    _size = 0
  }

  /** Sets the `index`th element of this buffer to `elem`.
    * @throws IndexOutOfBoundsException if `index` is not in `[0,size)`. */
  def update (index :Int, elem :A) :Unit = {
    Seq.checkIndex(index, _size)
    _elems(index) = elem
  }

  /** Appends `elem` to this buffer, expanding the buffer as necessary. */
  def append (elem :A) :this.type = {
    val size = _size
    expect(size, 1)(size) = elem
    _size = size+1
    this
  }
  /** Appends `elem` to this buffer, expanding the buffer as necessary. */
  def += (elem :A) :Unit = append(elem)

  /** Appends `iter` to this buffer. Consumes the iterator in the process. */
  def append (iter :JIterator[_ <: A]) :this.type = {
    var sz = _size
    while (iter.hasNext) { expect(sz, 1)(sz) = iter.next ; sz += 1 }
    _size = sz
    this
  }
  /** See [[append]]. */
  def ++= (iter :JIterator[_ <: A]) :Unit = append(iter)

  /** Appends `elems` to this buffer. */
  def append (elems :JIterable[_ <: A]) :this.type = elems match {
    // take a little peek under the covers in the name of efficiency
    case uu :Unordered[A] => append(uu)
    case cc :Collection[A] =>
      var sz = _size ; val iter = cc.iterator()
      val elems = expect(sz, cc.size)
      while (iter.hasNext) { elems(sz) = iter.next ; sz += 1 }
      _size = sz
      this
    case _ => append(elems.iterator())
  }
  /** See [[append]]. */
  def ++= (elems :JIterable[_ <: A]) :Unit = append(elems)

  /** Appends `elems` to this buffer. */
  def append (elems :Unordered[A]) :this.type = {
    val esize = elems.size ; val size = _size
    elems.copyInto(expect(size, esize), size)
    _size = size + esize
    this
  }
  /** See [[append]]. */
  def ++= (elems :Unordered[A]) :Unit = append(elems)

  /** Appends `elems` to this buffer. */
  def append (elems :SIterable[A]) :this.type = {
    val esize = elems.knownSize
    if (esize < 0) elems foreach append
    else {
      val size = _size
      elems.copyToArray(expect(size, esize), size, esize)
      _size = size + esize
    }
    this
  }
  /** See [[append]]. */
  def ++= (elems :SIterable[A]) :Unit = append(elems)

  /** Prepends `elem` to this buffer, shifting all elements down by one. */
  def prepend (elem :A) :Unit = insert(0, elem)

  /** Prepends `elems` to this buffer, shifting all elements down by `elems.size`. */
  def prepend (elems :Unordered[A]) :Unit = insert(0, elems)

  /** Inserts `elem` at `index`, expanding the buffer to accommodate, and shifting all elements
    * after `index` down by one. */
  def insert (index :Int, elem :A) :Unit = {
    val size = _size
    expectAt(index, 1, size)(index) = elem
    _size = size + 1
  }

  /** Inserts `elems` at `index`, expanding the buffer to accommodate, and shifting all elements
    * after `index` down by `elems.size`. */
  def insert (index :Int, elems :Unordered[A]) :Unit = {
    val size = _size ; val esize = elems.size
    elems.copyInto(expectAt(index, esize, size), index)
    _size = size + esize
  }

  /** Removes `count` elements starting at `index`, shifting any later elements up by `count`.
    * @throws IndexOutOfBoundsException if `[index,index+count)` is not in `[0,size)`. */
  def remove (index :Int, count :Int) :Unit = {
    if (count > 0) {
      val size = _size ; val off = index+count
      Seq.checkBounds(index, off, size)
      val elems = _elems.asInstanceOf[Array[Object]]
      if (off < size) System.arraycopy(elems, off, elems, index, size-off)
      Arrays.fill(elems, size-count, size, null)
      _size -= count
    }
  }

  /** Removes and returns the single element at `index`.
    * @throws IndexOutOfBoundsException if `[index,index+1)` is not in `[0,size)`. */
  def removeAt (index :Int) :A = {
    val elem = get(index)
    remove(index, 1)
    elem
  }

  /** Removes the first element which is equal to `elem` (per [[Object.equals]]). Returns true if
    * an element was found and removed, false otherwise. */
  def remove (elem :A) :Boolean = indexOf(elem) match {
    case -1 => false
    case ii => remove(ii, 1) ; true
  }
  /** An alias for [[remove]]. */
  def -= (elem :A) :Boolean = remove(elem)

  /** Removes the first `count` elements from this buffer. If `count` exceeds [[size]], the buffer
    * is cleared. */
  def trimStart (count :Int) :Unit = if (count > size) clear() else remove(0, count)

  /** Removes the last `count` elements from this buffer. If `count` exceeds [[size]], the buffer
    * is cleared. */
  def trimEnd (count :Int) :Unit = if (count > size) clear() else remove(size-count, count)

  override def clone :SeqBuffer[A] = new SeqBuffer[A](_elems.clone(), _size)

  override def get (index :Int) = {
    Seq.checkIndex(index, _size)
    _elems(index).asInstanceOf[A]
  }

  override def size = _size

  override def copyInto (start :Int, end :Int, target :Array[Any], offset :Int) :Unit = {
    Seq.checkBounds(start, end, _size)
    System.arraycopy(_elems, start, target, offset, end-start)
  }

  override def iterator () :JIterator[A @uV] = new JIterator[A]() {
    private var _index = 0
    override def hasNext :Boolean = _index < size
    override def next :A = {
      if (_index < size) try { get(_index) } finally { _index += 1 }
      else throw new NoSuchElementException()
    }
  }

  override def hashCode () = Std.hashCode(_elems.asInstanceOf[Array[Object]], _size)

  override protected def toStringType = "SeqBuffer"

  /** Returns the internal elements array for this buffer, nulling it out from this buffer in the
    * process. This allows a SeqBuffer to be used by builders and then rendered unusable. */
  protected def elemsForBuild :Array[Any] = try _elems finally { _elems = null }
}

object SeqBuffer {

  /** Creates a new buffer with the specified initial capacity. */
  def withCapacity[A] (initCapacity :Int) = new SeqBuffer[A](initCapacity)

  /** Creates a new buffer with the default initial capacity (16). */
  def apply[A] () :SeqBuffer[A] = withCapacity(16)

  /** Creates a new buffer wit the specified initial elements. */
  def apply[A] (elems :A*) :SeqBuffer[A] = withCapacity(elems.size).append(elems)
}
