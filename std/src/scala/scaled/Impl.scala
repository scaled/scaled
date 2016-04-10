//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A bunch of implementation details that you should ignore, but can make use of if you're
  * extending the library with your own reactive classes.
  */
object Impl {

  /** Implements [[Connection]] and a cons list for [[Reactor]]s. */
  class Link (
    /** The reactor that owns this cons cell. */
    val owner :Reactor,
    /** The priority of this connection. */
    val priority :Int
  ) extends Connection {
    /** The next connection in our chain. */
    var next :Link = _
    /** Indicates whether this connection is one-shot or persistent. */
    var oneShot :Boolean = false

    def setNext (next :Link) :Link = { this.next = next; this }

    // plumbing used (or not) by reactors; dangerous but simple and reasonably fast
    def notify (arg0 :Any) {}
    def notify (arg0 :Any, arg1 :Any) {}

    override def once () = { oneShot = true; this }
    override def close () = owner.disconnect(this)
    override def toString =
      s"[owner=$owner, prio=$priority, hasNext=${ next != null }, oneShot=$oneShot]"
  }

  abstract class Runs extends Runnable {
    var next :Runs = _
  }

  final val DISPATCHING = new Link(null, 0)

  def insert (head :Link, cons :Link) :Link = {
    if (head == null) cons
    else if (head.priority < cons.priority) cons.setNext(head)
    else head.setNext(insert(head.next, cons))
  }

  def remove (head :Link, cons :Link) :Link = {
    if (head == null) head
    else if (head == cons) head.next
    else head.setNext(remove(head.next, cons))
  }

  def insert (head :Runs, action :Runs) :Runs = {
    if (head == null) action
    else {
      head.next = insert(head.next, action)
      head
    }
  }
}
