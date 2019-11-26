//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Encapsulates a "visit". */
abstract class Visit {

  /** Visits our target in `window`'s focused frame.
    * @param push whether to push the current location onto the visit stack before we go. */
  def apply (window :Window, push :Boolean = true) :Unit = {
    val obuf = window.focus.view.buffer ; val oloc = window.focus.view.point()
    go(window)
    // if requested, and we actually went anywhere, push our old loc onto the visit stack
    if (push && (oloc != window.focus.view.point() || obuf != window.focus.view.buffer)) {
      window.visitStack.push(obuf, oloc)
    }
  }

  protected def go (window :Window) :Unit
}

/** Visit factory methods. */
object Visit {

  /** Contains a list of locations that may be visited in turn. A location is usually a file/offset
    * pair, but can be any kind of "visit" that makes sense. The user can navigate through a visit
    * list by pressing the keys bound to `visit-next` and `visit-prev`.
    *
    * Any time a mode generates a list of locations that the user is likely to want to navigate
    * through, it can set that as the window's visit list and the user can navigate the list using
    * a comfortable, standard set of key bindings.
    *
    * @param thing a noun describing the thing being visited ("error", "test failure", etc.).
    */
  class List (val thing :String, val visits :SeqV[Visit]) {

    /** Whether this list is empty. */
    def isEmpty = visits.isEmpty

    /** Visits the next visit in the list in `frame`. */
    def next (window :Window) :Unit = {
      if (isEmpty) window.popStatus(onNone)
      else {
        _current += 1
        if (_current < visits.size) go(window)
        else {
          _current = -1
          window.emitStatus(atLast)
        }
      }
    }

    /** Visits the previous visit in the list in `frame`. */
    def prev (window :Window) :Unit = {
      if (isEmpty) window.popStatus(onNone)
      else if (_current == -1) {
        _current = visits.size-1
        go(window)
      } else if (_current == 0) {
        _current = -1
        window.emitStatus(atFirst)
      } else {
        _current -= 1
        go(window)
      }
    }

    /** Creates a new visit list, setting its position to this list's current position if the new
      * list's `thing` matches this list's and it contains this list's current element. */
    def update (thing :String, visits :SeqV[Visit]) :List = {
      val updated = new List(thing, visits)
      if (thing == this.thing && _current != -1) {
        updated._current = visits.indexOf(this.visits(_current))
        updated._lastLoc = _lastLoc
      }
      updated
    }

    private var _current = -1
    private var _lastLoc = Loc.None

    private def go (window :Window) :Unit = {
      // if we're not sitting on the position that we last visited, push the current view+loc onto
      // the visit stack; this means that the first time you start cycling through the visit list,
      // we note your loc, but if you just cycle cycle cycle, we don't push every intermediate
      // location onto the visit stack, only if you move the point after a visit do we push
      visits(_current)(window, window.focus.view.point() != _lastLoc)
      _lastLoc = window.focus.view.point()
    }

    protected def things = thing + "s"
    protected def onNone  = s"No $things."
    protected def atFirst = s"At first $thing. Repeat command to start from last $thing."
    protected def atLast  = s"At last $thing. Repeat command to start from first $thing."
  }

  /** Maintains a stack of `(store, loc)` visits. The [[Window]] maintains a global visit stack
    * which is used with the visit list mechanism to track where the user was before they began
    * cycling through the visit list.
    *
    * A mode can also push the current location onto the visit stack whenever it is triggering a
    * jump to some other buffer location that is conceptually similar to an entry in the visit
    * stack. The user can then use a single key binding to "pop" back from their visits to their
    * previous locations.
    */
  class Stack {

    /** Pushes the buffer and point for `view` on the stack. */
    def push (view :BufferView) :Unit = push(view.buffer, view.point())

    /** Pushes `buffer.store` and `loc` onto the stack. */
    def push (buffer :BufferV, loc :Loc) :Unit = push(buffer.store, loc)

    /** Pushes `(store, loc)` onto the visit stack. */
    def push (store :Store, loc :Loc) :Unit = {
      _stack += (store -> loc)
    }

    /** Pops the last `(store, loc)` from the stack and visits it. */
    def pop (window :Window) :Unit = {
      if (_stack.isEmpty) window.emitStatus(s"Visit stack is empty.")
      else {
        val (store, loc) = _stack.last
        _stack.removeAt(_stack.size-1)
        window.focus.visitFile(store).point() = loc
      }
    }

    private val _stack = SeqBuffer[(Store,Loc)]()
  }

  /** Used to tag lines in buffers with visits. `help-mode` supports automatically visiting a
    * line's visit when ENTER is pressed on said line. Other modes can also use the tag. */
  case class Tag (visit :Visit) extends Line.Tag {
    /** Applies our enclosed visit. */
    def apply (window :Window, push :Boolean = true) :Unit = visit.apply(window, push)
  }

  /** Returns an instance that will visit `offset` in `store`. */
  def apply (store :Store, offset :Int) :Visit = new Visit() {
    protected def go (window :Window) :Unit = {
      val view = window.focus.visitFile(store)
      view.point() = view.buffer.loc(offset)
    }
    override def toString = s"Visit($store, $offset)"
  }

  /** Returns an instance that will visit `loc` in `store`. */
  def apply (store :Store, loc :Loc) :Visit = new Visit() {
    protected def go (window :Window) :Unit = {
      val view = window.focus.visitFile(store)
      view.point() = loc
    }
    override def toString = s"Visit($store, $loc)"
  }
}
