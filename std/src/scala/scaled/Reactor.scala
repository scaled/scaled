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
class Reactor {
  import Impl._

  /** Returns true if this reactor has at least one connection. */
  def hasConnections :Boolean = (_listeners != null)

  // protected def addConnection (prio :Int, listener :AnyRef) :Connection = synchronized {
  //   if (listener == null) throw new NullPointerException("Null listener")
  //   addLink(new Link(this, prio, listener))
  // }

  protected def addLink (cons :Link) :Link = synchronized {
    if (isDispatching) {
      _pendingRuns = insert(_pendingRuns, new Runs {
        def run () :Unit = {
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

  protected def prepareNotify () :Link = synchronized {
    if (isDispatching)
      throw new IllegalStateException("Initiated notify while notifying")
    val lners = _listeners
    _listeners = DISPATCHING.asInstanceOf[Link]
    return lners
  }

  protected def finishNotify (lners :Link) = synchronized {
    // note that we're no longer dispatching
    _listeners = lners

    // now remove listeners any queued for removing and add any queued for adding
    while (_pendingRuns != null) {
      _pendingRuns.run()
      _pendingRuns = _pendingRuns.next
    }
  }

  protected[scaled] def disconnect (cons :Link) = synchronized {
    if (isDispatching) {
      _pendingRuns = insert(_pendingRuns, new Runs {
        def run () :Unit = {
          _listeners = remove(_listeners, cons)
          connectionRemoved()
        }
      })
    } else {
      _listeners = remove(_listeners, cons)
      connectionRemoved()
    }
  }

  protected def clearListeners () :Unit = {
    _listeners = null
  }

  protected def shortClassName :String = {
    val cname = getClass.getName
    cname.substring(cname.lastIndexOf(".")+1)
  }

  /** Called prior to mutating any underlying model allows subclasses to reject mutation. */
  protected def checkMutate () :Unit = {} // noop

  /** Called when a connection has been added to this reactor. */
  protected def connectionAdded () :Unit = {} // noop

  /** Called when a connection may have been removed from this reactor. */
  protected def connectionRemoved () :Unit = {} // noop

  // always called while lock is held on this reactor
  private final def isDispatching :Boolean = (_listeners eq DISPATCHING)

  private[this] var _listeners :Link = _
  private[this] var _pendingRuns :Runs = _
}
