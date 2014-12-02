//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A base class for all reactive classes. This is an implementation detail, but is public so that
  * third parties may use it to create their own reactive classes, if desired.
  *
  * @define PRIODOC the priority of the connection. Higher priorities are notified first.
  * @define CONDOC an object that can be used to close the connection.
  * @define EXNDOC ReactionException encapsulates any errors thrown by listeners during notification.
  * All listeners will be notified regardless of whether any throw exceptions, and if one or more
  * listeners throw exceptions, they are aggregated into a ReactionException and thrown.
 */
class Reactor[L] {
  import Impl._

  /** Returns true if this reactor has at least one connection. */
  def hasConnections :Boolean = (_listeners != null)

  protected def addConnection (prio :Int, listener :L) :Connection = synchronized {
    if (listener == null) throw new NullPointerException("Null listener")
    addCons(new Cons(this, prio, listener))
  }

  protected def addCons (cons :Cons[L]) :Cons[L] = synchronized {
    if (isDispatching) {
      _pendingRuns = insert(_pendingRuns, new Runs {
        def run () {
          _listeners = insert(_listeners, cons)
          connectionAdded()
        }
      })
    } else {
      _listeners = insert(_listeners, cons)
      connectionAdded()
    }
    cons
  }

  protected def prepareNotify () :Cons[L] = synchronized {
    if (isDispatching)
      throw new IllegalStateException("Initiated notify while notifying")
    val lners = _listeners
    _listeners = DISPATCHING.asInstanceOf[Cons[L]]
    return lners
  }

  protected def finishNotify (lners :Cons[L]) = synchronized {
    // note that we're no longer dispatching
    _listeners = lners

    // now remove listeners any queued for removing and add any queued for adding
    while (_pendingRuns != null) {
      _pendingRuns.run()
      _pendingRuns = _pendingRuns.next
    }
  }

  protected[scaled] def disconnect (cons :Cons[L]) = synchronized {
    if (isDispatching) {
      _pendingRuns = insert(_pendingRuns, new Runs {
        def run () {
          _listeners = remove(_listeners, cons)
          connectionRemoved()
        }
      })
    } else {
      _listeners = remove(_listeners, cons)
      connectionRemoved()
    }
  }

  protected def removeConnection (listener :AnyRef) = synchronized {
    if (isDispatching) {
      _pendingRuns = insert(_pendingRuns, new Runs {
        def run () {
          _listeners = removeAll(_listeners, listener)
          connectionRemoved()
        }
      })
    } else {
      _listeners = removeAll(_listeners, listener)
      connectionRemoved()
    }
  }

  protected def clearListeners () {
    _listeners = null
  }

  protected def shortClassName :String = {
    val cname = getClass.getName
    cname.substring(cname.lastIndexOf(".")+1)
  }

  /** Called prior to mutating any underlying model allows subclasses to reject mutation. */
  protected def checkMutate () {} // noop

  /** Called when a connection has been added to this reactor. */
  protected def connectionAdded () {} // noop

  /** Called when a connection may have been removed from this reactor. */
  protected def connectionRemoved () {} // noop

  // always called while lock is held on this reactor
  private final def isDispatching :Boolean = (_listeners eq DISPATCHING)

  private[this] var _listeners :Cons[L] = _
  private[this] var _pendingRuns :Runs = _
}
