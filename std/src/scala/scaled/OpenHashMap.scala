//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{Arrays, Objects}

/**
 * An open-addressing hash table implementation of [[Map]]. This delegates all the hard work to
 * [[OpenHashSet]] and simply maintains an array of values at the same indices as the set keys.
 */
class OpenHashMap[K,+V] (_keys :OpenHashSet[K], _values :Array[Any]) extends Map[K,V] {

  override def size = _keys.size

  override def apply (key :K) :V = _keys.getIdx(key) match {
    case -1 => throw new NoSuchElementException(String.valueOf(key))
    case ii => getAt(ii)
  }
  override def get (key :K) :Option[V] = _keys.getIdx(key) match {
    case -1 => None
    case ii => Some(getAt(ii))
  }
  override def contains (key :K) :Boolean = _keys.contains(key)

  override def keySet = _keys
  override def values = new Seq[V](_values, size)

  override def foreach[U] (op :(K, V) => U) :Unit = {
    var ii = 0 ; val ss = size ; while (ii < ss) {
      op(_keys.getAt(ii), getAt(ii))
      ii += 1
    }
  }

  override def iterator () :JIterator[(K,V) @uV] = new JIterator[(K,V)]() {
    private var _index = 0
    override def hasNext :Boolean = _index < size
    override def next :(K,V) = {
      val ii = _index
      if (ii >= size) throw new NoSuchElementException()
      else { _index += 1 ; (_keys.getAt(ii), getAt(ii)) }
    }
  }

  override def hashCode = {
    var code = 1
    var ii = 0 ; val ll = size ; while (ii < ll) {
      val kcode = _keys.getAt(ii).hashCode
      val vcode = getAt(ii).hashCode
      code = 31 * code + kcode ^ vcode
      ii += 1
    }
    code
  }

  override def equals (other :Any) :Boolean = other match {
    case omap :OpenHashMap[_,_] =>
      if (omap.size != size) false
      else {
        var ii = 0 ; val ll = size ; while (ii < ll) {
          if (!omap.elemEquals(_keys.getAt(ii), getAt(ii))) return false
          ii += 1
        }
        true
      }
    case omap :Map[K,V] => omap equals this
    case _ => false
  }

  private final def elemEquals (key :Any, value :Any) :Boolean = _keys.getIdx(key) match {
    case -1 => false
    case ii => Objects.equals(getAt(ii), value)
  }
  private final def getAt (ii :Int) :V = _values(ii).asInstanceOf[V]
}

object OpenHashMap {

  /** Used to build [[OpenHashSet]]s. */
  class Builder[K,V] (esize :Int) extends Map.Builder[K,V] {

    private var _values = new Array[Any](math.max(esize, 2))
    private val _ksb = new OpenHashSet.Builder[K](esize) {
      override protected def didExpand (index :Int, count :Int, ncap :Int) :Unit = {
        // if we just shifted, then match the shift
        val sz = size ; val vals = _values ; val remain = sz-index
        if (ncap == vals.length) {
          if (remain > 0) System.arraycopy(vals, index, vals, index+count, remain)
        } else {
          val nvals = new Array[Any](ncap)
          System.arraycopy(vals, 0, nvals, 0, index)
          if (remain > 0) System.arraycopy(vals, index, nvals, index+count, remain)
          _values = nvals
        }
      }
      override protected def didCompact (ii :Int, nn :Int) :Unit = {
        _values(nn) = _values(ii)
      }
      override protected def sawDuplicate (oo :Int, dd :Int) :Unit = {
        // we want to use the last mapping for a given key, so if we see a duplicate, swap the
        // value for the duplicate key into the original key's position
        _values(oo) = _values(dd)
      }
    }

    override def build () :Map[K,V] = {
      val keySet = _ksb.build()
      new OpenHashMap[K,V](keySet, OpenHashSet.shrink(_values, keySet.size))
    }

    override def put (key :K, value :V) :this.type = {
      val ii = _ksb.size
      _ksb += key
      _values(ii) = value
      this
    }

    override def += (value :(K,V)) :Unit = append(value)
    override def ++= (elems :SIterable[(K,V)]) = append(elems)
    override def ++= (elems :Unordered[(K,V)]) = append(elems)
    override def ++= (elems :JIterable[_ <: (K,V)]) = append(elems.iterator())
    override def ++= (iter :JIterator[_ <: (K,V)]) = append(iter)
    override def append (elems :SIterable[(K,V)]) = { elems foreach append ; this }
    override def append (elems :Unordered[(K,V)]) = { elems foreach append ; this }
    override def append (elems :JIterable[_ <: (K,V)]) = append(elems.iterator())
    override def append (iter :JIterator[_ <: (K,V)]) = {
      while (iter.hasNext) append(iter.next) ; this
    }
    override def append (value :(K,V)) = put(value._1, value._2)
  }
}
