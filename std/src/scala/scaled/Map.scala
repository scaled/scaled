//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{AbstractMap, Map => JMap, Objects, HashSet}

/** A mapping from keys to values. Conceptually this is a `Set[K]` and `(K => V)`. */
abstract class Map[K,+V] extends Unordered[(K,V)] with (K => V) {

  /** Returns the number of mappings in this map. */
  def size :Int

  /** Returns the mapping for `key` or throws an exception.
    * @throws NoSuchElementException if no mapping exists for `key`. */
  def apply (key :K) :V

  /** Returns `Some` mapping for `key` or `None`. */
  def get (key :K) :Option[V]

  /** Returns true if a mapping exists for `key`, false otherwise. */
  def contains (key :K) :Boolean

  /** Returns the set of keys for this map. */
  def keySet :Set[K]

  /** Returns the values in this map as an unordered collection. */
  def values :Unordered[V]

  /** Creates a new map containing this map's mappings plus the mappings in `that`. */
  def concat[V1 >: V] (that :Iterable[(K,V1)]) :Map[K,V1] = {
    val mb = Map.builder[K,V1](size + that.sizeHint)
    foreach(mb.put(_,_))
    mb ++= that
    mb.build()
  }
  /** An alias for [[concat(Iterable)]] for use by Java. Scala code should use [[concat(Iterable)]]
    * as it does the right thing with regard to variance. */
  def concat[V1 >: V] (that :JIterable[(K,V1)]) :Map[K,V1] = concat(that :Iterable[(K,V1)])
  /** An alias for [[concat]]. */
  def ++[V1 >: V] (bs :JIterable[(K,V1)]) :Map[K,V1] = concat(bs)

  /** Applies `op` to each mapping in this map. As op takes two arguments, this avoids boxing each
    * map entry into a tuple. */
  def foreach[U] (op :(K, V) => U) :Unit

  /** Applies `f` to each `(k,v)` mapping in this map and returns a [[Seq]] that contains the
    * concatenation of the results. */
  def flatMap[B] (f :((K,V)) => JIterable[B]) :Seq[B] = foldBuild[B]((b, a) => b ++= f(a)).toSeq

  /** Applies `f` to each `(k,v)` mapping in this map and returns a [[Seq]] that contains the
    * concatenation of the results. */
  def flatMap[B] (f :(K,V) => JIterable[B]) :Seq[B] = {
    val b = newBuilder[B](4)
    foreach { (k,v) => b ++= f(k, v) }
    b.build()
  }

  /** Returns a [[Seq]] which contains `f` applied to each `(k,v)` mapping. */
  def map[B] (f :(K,V) => B) :Seq[B] = {
    val b = newBuilder[B](size)
    foreach { (k,v) => b += f(k, v) }
    b.build()
  }

  // views

  /** Returns a view of this seq as a [[JMap]]. */
  def asJMap[V1 >: V] :JMap[K,V1] = new AbstractMap[K,V1]() {
    override def isEmpty = Map.this.isEmpty
    override def size = Map.this.size
    override def get (key :Any) = Map.this.get(key.asInstanceOf[K]) || null.asInstanceOf[V1]
    override def containsKey (key :Any) = Map.this.contains(key.asInstanceOf[K])
    override def keySet = Map.this.keySet.asJSet
    override def values = Map.this.values.toSeq.asJList[V1]
    override def entrySet = {
      val set = new HashSet[JMap.Entry[K,V1]]()
      Map.this foreach { (k,v) => set.add(new AbstractMap.SimpleImmutableEntry(k, v)) }
      set
    }
  }

  // TODO: mapValues, etc.

  // overrides for performance and type specificity

  override def filter (pred :((K,V)) => Boolean) :Map[K,V] = super.filter(pred).toMap
  override def filterNot (pred :((K,V)) => Boolean) :Map[K,V] = super.filterNot(pred).toMap
  // override def foldBuild[B] (op :(Unordered.Builder[B],A) => Unit) :Set[B] =
  //   super.foldBuild(op).toSet
  // override def map[B] (f :A => B) :Set[B] = super.map(f).toSet

  // prevent Function1 from usurping our toString
  override def toString = super[Unordered].toString

  override def equals (that :Any) = that match {
    case omap :Map[K,V] => (omap.size == size) && (
      keySet forall { k => omap.contains(k) && Objects.equals(apply(k), omap.apply(k)) })
    case _ => false
  }

  override def newBuilder[B] (expectedSize :Int) = Seq.builder[B](expectedSize)
  override def newEmpty[B] = Seq.empty
  override protected def toStringType = "Map"
}

object Map {

  /** Used to build [[Map]]s. */
  trait Builder[K,V] extends Unordered.Builder[(K,V)] {
    /** Adds `key - value` to the accumulating map. If a previous mapping for `key` was added to
      * this builder, it will be replaced. */
    def put (key :K, value :V) :this.type
    /** An alias for [[put]]. */
    def += (key :K, value :V) :this.type = put(key, value)

    /** Builds the map based on the previously added mappings. */
    override def build () :Map[K,V]
  }

  /** Returns a [[Map]] builder. */
  def builder[K,V] () :Builder[K,V] = builder(4)

  /** Returns a [[Map]] builder prepared to build a list with at least `expectedSize` elements. */
  def builder[K,V] (expectedSize :Int) :Builder[K,V] = new OpenHashMap.Builder[K,V](expectedSize)

  /** Returns the empty map. */
  def empty[K,V] :Map[K,V] = EMPTY.asInstanceOf[Map[K,V]]

  /** Returns the empty map. */
  def apply[K,V] () :Map[K,V] = empty
  /** Returns a map containing the supplied single mapping. */
  def apply[K,V] (e0 :(K,V)) :Map[K,V] =
    builder[K,V](1).append(e0).build()
  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (e0 :(K,V), e1 :(K,V)) :Map[K,V] =
    builder[K,V](2).append(e0).append(e1).build()
  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (e0 :(K,V), e1 :(K,V), e2 :(K,V)) :Map[K,V] =
    builder[K,V](3).append(e0).append(e1).append(e2).build()
  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (e0 :(K,V), e1 :(K,V), e2 :(K,V), e3 :(K,V)) :Map[K,V] =
    builder[K,V](4).append(e0).append(e1).append(e2).append(e3).build()
  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (e0 :(K,V), e1 :(K,V), e2 :(K,V), e3 :(K,V), rest :(K,V)*) :Map[K,V] =
    builder[K,V](4+rest.size).append(e0).append(e1).append(e2).append(e3).append(rest).build()

  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (pairs :Unordered[(K,V)]) :Map[K,V] = {
    val b = builder[K,V](pairs.size)
    val iter = pairs.iterator() ; while (iter.hasNext) b.append(iter.next)
    b.build()
  }

  /** Returns a map containing the supplied mappings. */
  def apply[K,V] (pairs :Array[(K,V)]) :Map[K,V] = {
    val b = builder[K,V](pairs.size)
    var ii = 0 ; while (ii < pairs.length) { b.append(pairs(ii)) ; ii += 1 }
    b.build()
  }

  /** Returns a [[Map]] view of `jmap`. `jmap` must not contain null keys or values.
    * `jmap` is also assumed to be effectively immutable for the lifetime of this view.
    * Violate this assumption at your peril. */
  def view[K,V] (jmap :JMap[K,V]) :Map[K,V] =
    if (jmap.isEmpty) empty else new Map[K,V]() {
      override def iterator () = new JIterator[(K,V) @uV]() {
        val eiter = jmap.entrySet.iterator()
        override def hasNext = eiter.hasNext
        override def next = { val e = eiter.next ; (e.getKey, e.getValue) }
      }
      override def apply (key :K) = jmap.get(key) match {
        case null => throw new NoSuchElementException(String.valueOf(key))
        case value => value
      }
      override def contains (key :K) = jmap.containsKey(key)
      override def get (key :K) = Option(jmap.get(key))
      override def keySet = Set.view(jmap.keySet)
      override def size = jmap.size
      override def values = Unordered.view(jmap.values)
      override def foreach[U] (op :(K, V) => U) {
        val eiter = jmap.entrySet.iterator() ; while (eiter.hasNext) {
          val e = eiter.next
          op(e.getKey, e.getValue)
        }
      }
    }

  private val EMPTY = new Map[Any,Nothing]() {
    override def iterator () = Iterables.EMPTY_ITER.asInstanceOf[JIterator[(Any,Nothing)]]
    override def size = 0
    override def apply (key :Any) = throw new NoSuchElementException(String.valueOf(key))
    override def get (key :Any) = None
    override def contains (key :Any) = false
    override def keySet = Set.empty
    override def values = Seq.empty
    override def foreach[U] (op :(Any, Nothing) => U) {}
  }
}
