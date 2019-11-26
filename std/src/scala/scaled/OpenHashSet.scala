//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.{Arrays, Comparator, Objects}

/** A [[Set]] implementation using open addressing and Robin Hood hashing. */
class OpenHashSet[A] (_table :Array[Int], _elems :Array[Any], _count :Int) extends Set[A] {

  override def contains (elem :A) = getIdx(elem) != -1

  override def copyInto (target :Array[Any], offset :Int) :Unit = {
    System.arraycopy(_elems, 0, target, offset, _count)
  }
  override def iterator () :JIterator[A @uV] = new JIterator[A]() {
    private var _index = 0
    override def hasNext = _index < size
    override def next = {
      val ii = _index
      if (ii >= size) throw new NoSuchElementException()
      else { _index += 1 ; getAt(ii) }
    }
  }
  override def size = _count

  override def hashCode = Std.hashCode(_elems.asInstanceOf[Array[Object]], _count)

  private[scaled] final def getAt (idx :Int) :A = _elems(idx).asInstanceOf[A]

  private[scaled] final def getIdx (key :Any) :Int = _table.length match {
    case 0  => -1 // empty set contains no keys
    case tt =>
      val tsize = tt/2 ; val mask = tsize-1
      val hash = key.hashCode
      // println(s"Seeking $key/$hash (${hash&mask}) / tsize=$tsize mask=$mask")
      @inline @tailrec def find (pos :Int, dist :Int) :Int = {
        val pos2 = pos*2 ; val oidx = _table(pos2)
        // if the slot is empty, the key is not in the table
        if (oidx == -1) -1
        else {
          // if we see an element with shorter probe distance than our current probe distance, the key
          // must not be in the table because otherwise we'd have swapped with this element on insert
          val ohash = _table(pos2+1)
          val odist = (pos + tsize - (ohash & mask)) & mask
          // println(s"Checking ($dist) $pos => ${elems(oidx)}/$ohash ($odist)")
          if (dist > odist) -1
          // maybe this is our element
          else if (hash == ohash && Objects.equals(key, _elems(oidx))) oidx
          // otherwise move forward to the next slot
          else find((pos + 1) & mask, dist+1)
        }
      }
      find(hash & mask, 0)
  }
}

object OpenHashSet {

  /** Used to build [[OpenHashSet]]s. */
  class Builder[A] (elems :Array[Any], count :Int) extends SeqBuffer[A](elems, count)
                                                   with Set.Builder[A] {
    def this (esize :Int) = this(new Array[Any](math.max(esize, 2)), 0)

    def buildSorted () :OpenHashSet[A] = {
      val elems = elemsForBuild ; val size = this.size
      Arrays.sort(elems.asInstanceOf[Array[Object]], 0, size)
      build(elems, size)
    }

    def buildSorted (cmp :Comparator[_ >: A]) :OpenHashSet[A] = {
      val elems = elemsForBuild ; val size = this.size
      Arrays.sort(elems.asInstanceOf[Array[Object]], 0, size, cmp.asInstanceOf[Comparator[Any]])
      build(elems, size)
    }

    override def build () :OpenHashSet[A] = build(elemsForBuild, size)

    private def build (elems :Array[Any], size :Int) = size match {
      case 0     => EMPTY.asInstanceOf[OpenHashSet[A]]
      case count =>
        // create an array of [idx, hash, idx, hash, ...]
        val tsize = tableSize(count, LoadFactor) ; val mask = tsize-1
        val table = new Array[Int](2 * tsize)
        Arrays.fill(table, -1)
        // println(s"Placing $count elems into table of size $tsize")

        // the outer loop proceeds over each element, finding its hash table position, or ignoring
        // it if it's a duplicate element; non-duplicate elements are condensed to the front of the
        // elements array in the process
        @tailrec @inline def loop (ii :Int, uniques :Int) :Int = if (ii == count) uniques else {
          val elem = elems(ii)
          // the inner loop finds the hash table position for a single element; it does the
          // robin-hooding: swapping the hash table position of our current element with an
          // existing element if our current element is farther from its ideal position (dist <
          // odist) than the existing element; this reduces variance for subsequent successful (and
          // failed) lookups
          @tailrec @inline def iloop (pos :Int, hash :Int, idx :Int, dist :Int) :Int = {
            val pos2 = pos*2
            val oidx = table(pos2)
            if (oidx == -1) {
              // println(s"Placing ${elems(idx)}/$hash ($dist) at $pos")
              table(pos2) = idx
              table(pos2+1) = hash
              uniques+1
            }
            else if (elem == elems(oidx)) {
              sawDuplicate(oidx, ii)
              uniques
            }
            else {
              // another element occupies our hash slot, check whether we should swap and continue
              val ohash = table(pos2+1)
              val odist = (pos + tsize - (ohash & mask)) & mask
              val npos = (pos + 1) & mask
              // if the existing element has probed further than we have, keep going
              if (odist >= dist) iloop(npos, hash, idx, dist+1)
              // otherwise swap with this element and move it further down the array
              else {
                // println(s"Replacing ${elems(oidx)}/$ohash ($odist) at $pos with " +
                //         s"${elems(idx)}/$hash ($dist)")
                table(pos2) = idx
                table(pos2+1) = hash
                iloop(npos, ohash, oidx, odist+1)
              }
            }
          }
          // move this element up the array to the next unused slot
          val idx = if (ii == uniques) ii else {
            elems(uniques) = elem
            didCompact(ii, uniques)
            uniques
          }
          val hash = elem.hashCode
          val nuniques = iloop(hash & mask, hash, idx, 0)
          loop(ii+1, nuniques)
        }

        // do the hashing, duplicate detection and (elems) array compaction
        val uniques = loop(0, 0)

        // if uniques is less than 75% the size of elems, trim it
        val shrunk = shrink(elems, uniques)

        // TODO: if tableSize(uniques) is smaller than table (by definition it will be half the size,
        // or smaller), shrink table and rehash everything (won't need to reshrink elems)
        new OpenHashSet[A](table, shrunk, uniques)
    }

    // callbacks that allow us to use this builder for OpenHashMap as well

    /** Called when we relocate an element in our source array. */
    protected def didCompact (ii :Int, nn :Int) :Unit = {}

    /** Called when we encounter a duplicate element. `oo` is the index of the original element
      * (which is retained), `dd` is the index of the duplicate element (which is discarded). */
    protected def sawDuplicate (oo :Int, dd :Int) :Unit = {}
  }

  /** Creates an [[OpenHashSet]] using the supplied initial elements array. This exists mainly to
    * efficiently create sets from Java varargs, normally one would use [[Builder]].
    *
    * The first `count` elements must be non-null. The initial elements may contain duplicates,
    * which will be filtered out. The returned set may take possession of `elems`, so do not mutate
    * it after passing it to this method. */
  def make[A] (elems :Array[Any], count :Int) :OpenHashSet[A] = new Builder(elems, count).build()

  // we can use a high load factor because of robin hood hashing
  private final val LoadFactor = 0.9f

  private[scaled] def shrink (elems :Array[Any], count :Int) :Array[Any] =
    if (count < 3*elems.length/4) {
      // oh scalac you can be so funny sometimes...
      Arrays.copyOf(elems.asInstanceOf[Array[Object]], count).asInstanceOf[Array[Any]]
    } else elems

  private def tableSize (size :Int, loadFactor :Float) =
    Integer.highestOneBit(Math.ceil(size / loadFactor).toInt) << 1 // next power of 2

  private final val EMPTY = new OpenHashSet[Any](new Array[Int](0), new Array[Any](0), 0)
}
