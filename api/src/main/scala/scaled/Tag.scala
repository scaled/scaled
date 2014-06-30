//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/** Tags a segment of a line with a value. This is used for CSS style spans, and also allows modes
  * to tag arbitrary segments of the buffer with data.
  *
  * Note that tags are resolved based on the runtime type of the tag value, and `String` is
  * reserved for CSS styles, so custom code must use its own classes when tagging line segments.
  */
abstract class Tag[T] {

  /** The value with which to tag the segment. */
  val tag :T

  /** The character offset of the start of the segment. */
  val start :Int

  /** The character offset of the end of the segment (non-inclusive). */
  val end :Int

  /** Returns the length of the segment tagged by this instance. */
  def length :Int = end - start

  /** Returns true if this tagged segment contains `idx`. */
  def contains (idx :Int) = (start <= idx) && (idx < end)

  /** Returns true if this tagged segment overlaps `[start, end)`. */
  def overlaps (start :Int, end :Int) =
    // TODO: this can probably be done more efficiently
    contains(start) || contains(end-1) || (start < this.start && end >= this.end)

  override def toString :String = s"${tag.getClass.getName}='$tag' @ $start-$end"
}

/** Maintains an ordered collection of tags for a line. Supports the various queries and mutations
  * that are needed by [[Line]] and [[Buffer]].
  *
  * We're mainly optimizing for minimal memory use: lookup time is linear in the number of tags on
  * the line. The expectation is that there will usually be no more than a couple of dozen tags per
  * line and that tag lookup will not be done at high frequency (versus syntax lookup which is used
  * during character by character searches of the buffer and which may be done thousands or tens of
  * thousands of times in a time slice). If tags turn out to be used substantially differently, we
  * can consider using a more complex data structure like a segment tree or interval tree, but that
  * complexity seemed overkill from the present perspective.
  *
  * Note regarding tag matching: tags are identified by their `Class` and `Class.isInstance` is
  * used to check tags for a match, so a hierarchy of tag sub-classes can be used under the
  * auspices of a single top-level tag class.
  */
class Tags {
  import Tags._

  // a dummy element at the root of our node linked list simplifies logic
  private val _root :Node[_] = rootNode()

  /** Adds a tag for the specified region denoted by `tag`. */
  def add[T] (tag :T, start :Int, end :Int) {
    _root.insert(mkNode(tag, start, end))
  }

  /** Returns the tags in this collection as a list. They are sorted by increasing `start`. */
  def tags :List[Tag[_]] = _root.toListTail

  /** Returns the first tag found at `idx` which matches `tclass`. */
  def tagAt[T] (tclass :Class[T], idx :Int, dflt :T) :T = {
    var node = _root.next ; while (node != null) {
      if (tclass.isInstance(node.tag) && node.contains(idx)) return node.tag.asInstanceOf[T]
      node = node.next
    }
    dflt
  }

  /** Returns all tags that intersect `idx` which match `tclass`. */
  def tagsAt[T] (tclass :Class[T], idx :Int) :List[T] = {
    var rs = Nil :List[T] ; var node = _root.next ; while (node != null) {
      if (tclass.isInstance(node.tag) && node.contains(idx)) rs = node.tag.asInstanceOf[T] :: rs
      node = node.next
    }
    rs
  }

  /** Returns all tags that intersect `idx`. */
  def tagsAt (idx :Int) :List[Tag[_]] = {
    var rs = Nil :List[Tag[_]] ; var node = _root.next ; while (node != null) {
      if (node.contains(idx)) rs = node :: rs
      node = node.next
    }
    rs
  }

  /** Visits tags which match `tclass` in order. `vis` will be called for each region which contains
    * a unique set of tags (including no tags). */
  def visit[T] (tclass :Class[T])(vis :(Seq[Tag[T]], Int, Int) => Unit) :Unit = try {
    var vts = Seq[Tag[T]]() ; var start = 0 ; var maxex = Int.MaxValue
    var node = _root.next ; while (node != null) {
      if (!tclass.isInstance(node.tag)) node = node.next
      else {
        // if we have no active visitation, start one with this tag; if we do have an active
        // visitation and this tag coincides with its start, add the tag
        if (vts.isEmpty || node.start == start) {
          vts :+= node.asInstanceOf[Tag[T]]
          start = node.start // NOOPs unless vts was empty
          maxex = math.min(maxex, node.end)
          node = node.next
        }
        // otherwise constrain our current visitation to the start of this tag, and do it
        else {
          maxex = math.min(maxex, node.start)
          vis(vts, start, maxex)
          // start a new visitation and filter out any tags that are done
          start = maxex
          // TODO: do this fold and filter at the same time...
          vts = vts.filter(_.end > start)
          maxex = (Int.MaxValue /: vts)((mx, vt) => math.min(vt.end, mx))
          // next time through the loop, we'll revisit this tag with a new start
        }
      }
    }
    // run any final visitations
    while (!vts.isEmpty) {
      vis(vts, start, maxex)
      // start a new visitation and filter out any tags that are done
      start = maxex
      // TODO: do this fold and filter at the same time...
      vts = vts.filter(_.end > start)
      maxex = (Int.MaxValue /: vts)((mx, vt) => math.min(vt.end, mx))
    }
  } catch {
    case t :Throwable =>
      println(s"Visit choked $this ($tags)")
      t.printStackTrace(System.out)
  }

  /** Removes `tag` from this collection. If `tag` exists but does not match the exact start and end
    * points, only the fragment which overlaps the to-be-removed region will be removed.
    * @return true if one or more tags were found and removed, false if no changes were made. */
  def remove[T] (tag :T, start :Int, end :Int) :Boolean = delete(_ == tag, start, end, 0)

  /** Removes all tags from this collection which overlap `[start, end)` and which match `tclass`
    * and `pred`. Note that unlike the single tag remove, this removes the entire tag even if only
    * part of it overlaps the region.
    * @return true if at least one tag was found and removed, false if no changes were made. */
  def removeAll[T] (tclass :Class[T], pred :T => Boolean, start :Int, end :Int) :Boolean = {
    var removed = false
    var pnode = _root ; var node = _root.next ; while (node != null && node.start < end) {
      if (tclass.isInstance(node.tag) && node.overlaps(start, end) &&
          pred(node.tag.asInstanceOf[T])) {
        pnode.next = node.next
        removed = true
      }
      pnode = node
      node = node.next
    }
    removed
  }

  // TODO: write checkInvariant() which ensures that tags are all in correct order; then call it
  // before and after every method (until we've ironed out all the bugs)

  /** Deletes tags in the specified region, chopping overlapping tags at the region and shifting
    * tags beyond the region `end-start` characters to the left. */
  def delete (start :Int, end :Int) :Unit = delete(null, start, end, end-start)

  /** Inserts a gap into the tag collection at `start`. Tags that overlap will be expanded,
    * tags that start at or after `start` will be shifted. */
  def expand (start :Int, length :Int) {
    var pnode = _root ; var node = _root.next ; while (node != null) {
      val nstart = node.start ; val nend = node.end
      @inline def replace (frag :Node[_]) = pnode.setNext(frag.setNext(node.next))
      // if the node starts at or after the expansion point, shift it
      if (nstart >= start) replace(mkNode(node.tag, nstart+length, nend+length))
      // if the node overlaps the expansion point, expand it
      else if (nend > start) replace(mkNode(node.tag, nstart, nend+length))
      // otherwise it starts and ends before the expansion point, ignore it
      pnode = pnode.next // TODO: write failing test for 'pnode = node'...
      node = node.next
    }
  }

  /** Deletes tags in the specified region, but does no shifting. */
  def clear (start :Int, end :Int) :Unit = delete(null, start, end, 0)

  /** Returns a 'slice' of this tags collection. Tags in the slice will be adjusted to be relative
    * to `start`, and tags which overlap the region will be truncated to fit into the region. */
  def slice (start :Int, end :Int) :Tags = {
    val tags = new Tags()
    sliceInto(start, end, tags, 0)
    tags
  }

  /** Slices `[start, end)` from these tags into `into` at `offset`. */
  def sliceInto (start :Int, end :Int, into :Tags, offset :Int) {
    def clip[T] (tag :T, cstart :Int, cend :Int) = if (cend > cstart) into.add(tag, cstart, cend)
    var node = _root.next ; while (node != null && node.start < end) {
      val nstart = node.start ; val nend = node.end
      // node starts at or before the region
      if (nstart <= start) {
        // if the node overlaps the region, slice out the middle
        if (nend >= end) clip(node.tag, offset, offset+end-start)
        // if the node overlaps on the left, include the overlappy bit
        else if (nend > start) clip(node.tag, offset, offset+nend-start)
        // otherwise it starts and ends before the region; ignore it
      }
      // node starts inside the region
      else if (nstart < end) {
        // take as much of the node as fits in the region
        clip(node.tag, offset+nstart-start, offset+math.min(nend, end)-start)
      }
      // otherwise node starts after region, ignore it
      node = node.next
    }
  }

  override def equals (other :Any) :Boolean = other.isInstanceOf[Tags] && (
    chaineq(_root, other.asInstanceOf[Tags]._root))

  private def delete (pred :Any => Boolean, start :Int, end :Int, shift :Int) :Boolean = {
    @inline @tailrec def loop (pnode :Node[_], mod :Boolean) :Boolean = {
      val node = pnode.next
      if (node == null) mod
      else if (pred != null && !pred(node.tag)) loop(node, mod)
      else {
        val nstart = node.start ; val nend = node.end
        // node starts before region
        if (nstart < start) {
          // if node also ends before region, it is not affected
          if (nend <= start) loop(node, mod)
          // if it ends inside the region, chop off the right
          else if (nend <= end) {
            val frag = mkNode(node.tag, node.start, start)
            pnode.setNext(frag.setNext(node.next))
            loop(frag, true)
          }
          else { // otherwise split it into before and after region parts
            val lfrag = mkNode(node.tag, node.start, start)
            val rfrag = mkNode(node.tag, end-shift, node.end-shift) // after part is shifted
            pnode.setNext(lfrag.setNext(rfrag.setNext(node.next)))
            loop(rfrag, true)
          }
        }
        // node starts inside region
        else if (nstart < end) {
          // if it also ends inside the region, just delete it
          if (nend <= end) loop(pnode.setNext(node.next), true)
          // otherwise retain the after part (and shift it)
          else {
            val frag = mkNode(node.tag, end-shift, nend-shift)
            pnode.setNext(frag.setNext(node.next))
            loop(frag, true)
          }
        }
        // node starts and ends after region, may need to be shifted
        else if (shift != 0) {
          val frag = mkNode(node.tag, nstart-shift, nend-shift)
          pnode.setNext(frag.setNext(node.next))
          loop(frag, true)
        }
        else loop(node, mod)
      }
    }
    loop(_root, false)
  }
}

object Tags {

  private def mkNode[T] (tag :T, start :Int, end :Int) = {
    if (start < 0) throw new IllegalArgumentException(s"start must be >= 0 ($tag, $start, $end)")
    if (end <= start) throw new IllegalArgumentException(s"end must be > start ($tag, $start, $end)")
    new Node(tag, start, end)
  }

  private def rootNode () = Node(null, Int.MinValue, Int.MaxValue)

  private case class Node[T] (tag :T, start :Int, end :Int) extends Tag[T] {
    var next :Node[_] = _

    def toList :List[Tag[_]] = this :: toListTail
    def toListTail :List[Tag[_]] = if (next == null) Nil else next.toList

    def insert (node :Node[_]) {
      if (next == null || node.start < next.start) next = node.setNext(next)
      else next.insert(node)
    }

    def setNext (node :Node[_]) :this.type = { next = node ; this }
  }

  private def chaineq (a :Node[_], b :Node[_]) :Boolean = a match {
    case null => b == null
    case a    => (a == b) && chaineq(a.next, b.next)
  }
}
