//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A view of a [[Signal]] on which one may listen but via which one cannot emit value.
  */
class SignalV[T] extends Reactor[T => Unit] {
  import Impl._

  /** Maps the output of this signal via `f`. When this signal emits a value, the mapped signal will
    * emit that value as transformed by `f`. The mapped value will retain a connection to this
    * signal for as long as it has connections of its own.
    */
  def map[M] (f :T => M) :SignalV[M] = {
    val outer = this
    new SignalV[M]() {
      // connectionAdded and connectionRemoved are only ever called with a lock held on this reactor,
      // so we're safe in checking and mutating _conn
      override protected def connectionAdded () {
        super.connectionAdded()
        if (_conn == null) _conn = outer.onValue(v => notifyEmit(f(v)))
      }
      override protected def connectionRemoved () {
        super.connectionRemoved()
        if (!hasConnections && _conn != null) {
          _conn.close()
          _conn = null
        }
      }
      protected var _conn :Connection = _
    }
  }

  /** Connects the supplied slot (side-effecting function) with priorty zero. When a value is
    * emitted, the slot will be invoked with the value.
    * @return $CONDOC
    */
  def onValue (slot :T => Unit) :Connection = addConnection(0, slot)

  /** Connects the supplied "value agnostic" block of code with priority 0. When a value is emitted,
    * the block will be executed. Useful when you don't care about the value.
    * @return $CONDOC
    */
  def onEmit (block : =>Unit) :Connection = addConnection(0, _ => block)

  /** Connects the supplied slot (side-effecting function) at the specified priority. When a value is
    * emitted, the slot will be invoked with the value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onValueAt (prio :Int)(slot :T =>Unit) :Connection = addConnection(prio, slot)

  /** Connects the supplied "value agnostic" block of code at the specified priority. When a value is
    * emitted, the block will be executed. Useful when you don't care about the value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onEmitAt (prio :Int)(block : =>Unit) :Connection = addConnection(prio, _ => block)

  /** Emits the supplied value to all connections. */
  protected def notifyEmit (value :T) {
    val lners = prepareNotify()
    var err :ReactionException = null
    try {
      var cons = lners
      while (cons != null) {
        try {
          cons.listener.apply(value)
        } catch {
          case t :Throwable =>
            if (err == null) err = new ReactionException()
            err addSuppressed t
        }
        if (cons.oneShot) cons.close()
        cons = cons.next
      }
    } finally {
      finishNotify(lners)
    }
    if (err != null) throw err
  }
}
