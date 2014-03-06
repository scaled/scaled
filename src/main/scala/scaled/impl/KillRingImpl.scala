//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scala.collection.mutable.ArrayBuffer

import scaled._

class KillRingImpl (size :Int) extends KillRing(size) {

  private val _entries = ArrayBuffer[Seq[Line]]()
  private var _pos = -1 // the position of the most recently added entry

  override def entry (index :Int) = if (_pos < 0) None else {
    val size = _entries.size
    Some(_entries((_pos + size - (index % size)) % size))
  }

  override def add (region :Seq[Line]) {
    assert(!region.isEmpty)
    _pos = (_pos + 1) % size
    if (_entries.size < size) _entries += region
    else _entries(_pos) = region
  }

  override def append (region :Seq[Line]) {
    if (_pos < 0) add(region)
    else _entries(_pos) = merge(_entries(_pos), region)
  }

  private def merge (as :Seq[Line], bs :Seq[Line]) =
    (as.dropRight(1) :+ as.last.merge(bs.head)) ++ bs.drop(1)
}
