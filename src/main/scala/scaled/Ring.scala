//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Manages a ring of killed (cut, in modern parlance) blocks of text (and styles). These blocks of
  * text can then be yanked from the kill ring back into a buffer (pasted). Being a ring, it
  * accumulates blocks until the ring is full, at which point adding new blocks causes the oldest
  * block to be replaced.
  *
  * @param size the maximum size of the kill ring.
  */
abstract class KillRing (val size :Int) {

  /** Returns `Some` kill-ring entry at `age` or `None` if the kill-ring is empty.
    * @param age the offset from the most recently added entry: 0 means the most recently added
    * entry, 1 means the entry added just prior to that, 2 means the entry prior to 1, etc. Indices
    * are taken modulo the kill ring's size, so a caller can request monotonically increasing ages
    * and eventually wrap around to the start of the ring.
    * @throws IndexOutOfBoundsException if a negative age is supplied. */
  def entry (age :Int) :Option[Seq[Line]]

  /** Adds `region` to the kill ring as a new entry. If the kill ring is full, the oldest entry will
    * be removed. */
  def add (region :Seq[Line]) :Unit

  /** Appends `region` to the youngest kill ring entry. */
  def append (region :Seq[Line]) :Unit

  /** Prepends `region` to the youngest kill ring entry. */
  def prepend (region :Seq[Line]) :Unit
}
