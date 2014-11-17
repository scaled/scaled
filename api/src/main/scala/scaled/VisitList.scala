//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Contains a list of locations that may be visited in turn. A location is usually a file/offset
  * pair, but can be any kind of "visit" that makes sense. The user can navigate through a visit
  * list by pressing the keys bound to `visit-next` and `visit-prev`.
  *
  * Any time a mode generates a list of locations that the user is likely to want to navigate
  * through, it can set that as the window's visit list and the user can navigate the list using a
  * comfortable, standard set of key bindings.
  *
  * @param thing a noun describing the thing visited in this list ("error", "test failure", etc.).
  */
class VisitList (thing :String, val visits :Seq[Visit]) {

  private var _current = -1

  /** Visits the next visit in the list in `frame`. */
  def next (window :Window) {
    if (visits.isEmpty) window.popStatus(onNone)
    else {
      _current += 1
      if (_current < visits.size) visits(_current)(window)
      else {
        _current = -1
        window.emitStatus(atLast)
      }
    }
  }

  /** Visits the previous visit in the list in `frame`. */
  def prev (window :Window) {
    if (visits.isEmpty) window.popStatus(onNone)
    else if (_current == -1) {
      _current = visits.size-1
      visits(_current)(window)
    } else if (_current == 0) {
      _current = -1
      window.emitStatus(atFirst)
    } else {
      _current -= 1
      visits(_current)(window)
    }
  }

  protected def things = thing + "s"
  protected def onNone  = s"No $things."
  protected def atFirst = s"At first $thing. Repeat command to start from last $thing."
  protected def atLast  = s"At last $thing. Repeat command to start from first $thing."
}

/** Encapsulates a "visit". */
abstract class Visit {

  /** Visits this visit's target in `window`'s focused frame. */
  def apply (window :Window) :Unit
}

/** Visit factory methods. */
object Visit {

  /** Returns an instance that will visit `offset` in `store`. */
  def apply (store :Store, offset :Int) :Visit = new Visit() {
    def apply (window :Window) {
      val view = window.focus.visitFile(store)
      view.point() = view.buffer.loc(offset)
    }
    override def toString = s"Visit($store, $offset)"
  }

  /** Returns an instance that will visit `loc` in `store`. */
  def apply (store :Store, loc :Loc) :Visit = new Visit() {
    def apply (window :Window) {
      val view = window.focus.visitFile(store)
      view.point() = loc
    }
    override def toString = s"Visit($store, $loc)"
  }
}
