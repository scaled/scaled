//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A bunch of implementation details that you should ignore, but can make use of if you're
  * extending the library with your own reactive classes.
  */
object Impl {

  /** Implements [[Connection]] and a linked-list style listener list for [[Reactor]]s.
    */
  class Cons[L](
    /** The reactor that owns this cons cell. */
    val owner :Reactor[L],
    /** The priority of this connection. */
    val priority :Int,
    /** Receives signals from the reactor. */
    val listener :L
  ) extends Connection {
    /** The next connection in our chain. */
    var next :Cons[L] = _
    /** Indicates whether this connection is one-shot or persistent. */
    var oneShot :Boolean = false

    def setNext (next :Cons[L]) :Cons[L] = { this.next = next; this }

    override def once () = { oneShot = true; this }
    override def close () = owner.disconnect(this)
    override def toString =
      s"[owner=$owner, prio=$priority, lner=$listener, hasNext=${ next != null }, oneShot=$oneShot]"
  }

  abstract class Runs extends Runnable {
    var next :Runs = _
  }

  final val DISPATCHING = new Cons(null, 0, null)

  def insert[L] (head :Cons[L], cons :Cons[L]) :Cons[L] = {
    if (head == null) cons
    else if (head.priority < cons.priority) cons.setNext(head)
    else head.setNext(insert(head.next, cons))
  }

  def remove[L] (head :Cons[L], cons :Cons[L]) :Cons[L] = {
    if (head == null) head
    else if (head == cons) head.next
    else head.setNext(remove(head.next, cons))
  }

  def removeAll[L] (head :Cons[L], listener :Object) :Cons[L] = {
    if (head == null) null
    else if (head.listener == listener) removeAll(head.next, listener)
    else head.setNext(removeAll(head.next, listener))
  }

  def insert (head :Runs, action :Runs) :Runs = {
    if (head == null) action
    else {
      head.next = insert(head.next, action)
      head
    }
  }
}
