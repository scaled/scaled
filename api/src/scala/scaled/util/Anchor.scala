//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.util.{ArrayList, Collections}
import scaled._

/** A location in a buffer which responds as predictably as possible to changes in the buffer.
  * Edits that precede the anchor cause it to shift forward or back appropriately. Edits after the
  * anchor do not cause movement.
  *
  * Deleting the text that includes the anchor causes the anchor to be removed and marked as
  * deleted. See [[shiftsOnDelete]] for alternate behavior.
  */
class Anchor private[util] (initLoc :Loc) extends Comparable[Anchor] {

  /** If true, this anchor will remain at its current location if an insert happens exactly at the
    * anchor position. If false, the anchor will shift to the end of the edit. */
  var bindsLeft :Boolean = false
  /** If true, causes the anchor to move to the start of the deleted region when it is included in a
    * deletion, rather than being marked as deleted. */
  var shiftsOnDelete :Boolean = false

  /** Sets [[bindsLeft]] to true and returns this anchor. */
  def bindLeft = { bindsLeft = true ; this }
  /** Sets [[bindsLeft]] to false and returns this anchor. */
  def bindRight = { bindsLeft = false ; this }
  /** Sets [[shiftsOnDelete]] to true and returns this anchor. */
  def shiftOnDelete = { shiftsOnDelete = true ; this }
  /** Returns true if this anchor has been deleted. */
  def isDeleted = _isDeleted

  /** Returns this anchor's current location. */
  def loc :Loc = _loc

  override def compareTo (other :Anchor) = _loc.compareTo(other.loc)

  private[util] def onInsert (start :Loc, end :Loc) {
    if (!bindsLeft || start < loc) _loc = Loc.adjustForInsert(_loc, start, end)
  }
  private[util] def onDelete (start :Loc, end :Loc) :Boolean = {
    // only delete an anchor if the deletion surrounds it, not if it starts exactly at the anchor
    if (!shiftsOnDelete && start < _loc && end > _loc) _isDeleted = true
    else _loc = Loc.adjustForDelete(_loc, start, end)
    _isDeleted
  }

  private[this] var _loc = initLoc
  private[this] var _isDeleted = false
}

/** Anchor stuffs. */
object Anchor {

  /** Maintains a set of [[Anchor]]s in a particular buffer. Because the anchor set must listen for
    * changes to the buffer and update the offsets of its anchors, its lifecycle must be managed.
    * If the anchor set is no longer needed but the buffer on which it operates will live longer,
    * the anchor set must be [[close]]d.
    */
  class Set (buffer :RBuffer) extends AutoCloseable {

    private[this] val _anchors = new ArrayList[Anchor]()

    private[this] val _conn = buffer.edited onValue {
      case Buffer.Insert(start, end) =>
        val iter = _anchors.iterator
        while (iter.hasNext) iter.next.onInsert(start, end)
      case Buffer.Delete(start, end, _) =>
        val iter = _anchors.iterator
        while (iter.hasNext) if (iter.next.onDelete(start, end)) iter.remove()
      case _ => // don't care about transform
    }

    /** Returns all anchors in this set, ordered by location. */
    val anchors :SeqV[Anchor] = _anchors.toSeqV

    /** Creates a new anchor and adds it to this set. */
    def add (loc :Loc) :Anchor = {
      val a = new Anchor(loc)
      val idx = Collections.binarySearch(_anchors, a)
      if (idx < 0) _anchors.add(-(idx+1), a)
      else _anchors.add(idx, a)
      a
    }

    /** Disconnects this anchor set from its buffer and renders it inert. */
    override def close () {
      _conn.close()
    }
  }

  /** Maintains a region which is bounded by two anchors. */
  import scaled.{Region => SRegion}
  case class Region (val startA :Anchor, val endA :Anchor) extends SRegion {
    /** This region's current start location. */
    def start = startA.loc
    /** This region's current end location. */
    def end = endA.loc

    /** Configures this region's anchors to "bind in", meaning they do not expand when insertions
      * happen at the region's edges. */
    def bindIn = { startA.bindRight ; endA.bindLeft ; this }
    /** Configures this region's anchors to "bind out", meaning they do expand when insertions
      * happen at the region's edges. */
    def bindOut = { startA.bindLeft ; endA.bindRight ; this }

    override def toString = SRegion.toString(this)
  }
}
