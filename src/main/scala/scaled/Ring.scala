//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import scala.collection.mutable.ArrayBuffer

/** Manages a ring of blocks of text (and styles). A ring accumulates blocks until the ring is
  * full, at which point adding new blocks causes the oldest block to be replaced. The canonical
  * use of a ring is the kill ring, which tracks killed (cut, in modern parlance) blocks of text
  * which can then be yanked from the kill ring back into a buffer (pasted).
  *
  * @param size the maximum size of the ring.
  */
class Ring (val size :Int) {

  private val _entries = ArrayBuffer[Seq[LineV]]()
  private var _pos = -1 // the position of the most recently added entry

  /** Returns `Some` ring entry at `age` or `None` if the ring is empty.
    *
    * @param age the offset from the most recently added entry: 0 means the most recently added
    * entry, 1 means the entry added just prior to that, 2 means the entry prior to 1, etc. Indices
    * are taken modulo the ring's size, so a caller can request monotonically increasing ages and
    * eventually wrap around to the start of the ring.
    *
    * @throws IndexOutOfBoundsException if a negative age is supplied. */
  def entry (age :Int) :Option[Seq[LineV]] = if (_pos < 0) None else {
    val size = _entries.size
    Some(_entries((_pos + size - (age % size)) % size))
  }

  /** Adds `region` to the ring as a new entry. If full, the oldest entry will be removed. */
  def add (region :Seq[LineV]) {
    assert(!region.isEmpty)
    _pos = (_pos + 1) % size
    if (_entries.size < size) _entries += region
    else _entries(_pos) = region
  }

  /** Appends `region` to the youngest ring entry. */
  def append (region :Seq[LineV]) {
    if (_pos < 0) add(region)
    else _entries(_pos) = merge(_entries(_pos), region)
  }

  /** Prepends `region` to the youngest ring entry. */
  def prepend (region :Seq[LineV]) {
    if (_pos < 0) add(region)
    else _entries(_pos) = merge(region, _entries(_pos))
  }

  protected def merge (as :Seq[LineV], bs :Seq[LineV]) =
    (as.dropRight(1) :+ as.last.merge(bs.head)) ++ bs.drop(1)
}
