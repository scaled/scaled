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

  /** Returns the number of elements in this ring. This will always be `<= size`. */
  def entries :Int = _entries.size

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

  /** Removes any regions from the ring that are equal to `region`, then [[add]]s `region`. */
  def filterAdd (region :Seq[LineV]) {
    var ii = _entries.indexOf(region)
    while (ii != -1) {
      _entries.remove(ii)
      if (ii <= _pos) _pos -= 1
      ii = _entries.indexOf(region, ii)
    }
    add(region)
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

/** A special ring that manages killed text. The chief specialization is that this ring
  * interoperates with the system clipboard. When data is added or appended to the kill ring, the
  * system clipboard is updated with the pasted text.
  */
class KillRing (size :Int) extends Ring(size) {
  import javafx.scene.input.{Clipboard, ClipboardContent}
  private val clipboard = Clipboard.getSystemClipboard
  private var lastSaved = ""

  override def entry (age :Int) :Option[Seq[LineV]] = {
    // if we're yanking the zeroth entry and the clipboard has something other than what we last
    // wrote to it, pull the contents of the clipboard into a new kill ring entry and return that
    if (age == 0 && clipboard.hasString()) {
      val clip = clipboard.getString()
      if (clip != lastSaved) add(Line.fromText(clip))
    }
    super.entry(age)
  }

  override def add (region :Seq[LineV]) {
    super.add(region)
    copyToClipboard(region)
  }

  override def append (region :Seq[LineV]) {
    super.append(region)
    copyToClipboard(entry(0).get)
  }

  override def prepend (region :Seq[LineV]) {
    super.prepend(region)
    copyToClipboard(entry(0).get)
  }

  private def copyToClipboard (region :Seq[LineV]) {
    val clip = new ClipboardContent()
    lastSaved = Line.toText(region)
    clip.putString(lastSaved)
    clipboard.setContent(clip)
  }
}
