//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

import java.util.function.{BiConsumer, Consumer, Function}

abstract class ValueReactor[T] extends Reactor {
  import Impl._

  /** Connects the supplied "value agnostic" block of code with priority 0. When a value is emitted,
    * the block will be executed. Useful when you don't care about the value.
    * @return $CONDOC
    */
  def onEmit (block : =>Unit) :Connection = onEmitAt(0)(block)

  /** Connects the supplied "value agnostic" block of code at the specified priority. When a value is
    * emitted, the block will be executed. Useful when you don't care about the value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onEmitAt (prio :Int)(block : =>Unit) :Connection = addLink(new Link(this, prio) {
    override def notify (value :Any, ovalue :Any) = block
  })

  /** Connects the supplied slot (side-effecting function) with priorty zero. When a value is
    * emitted, the slot will be invoked with the value.
    * @return $CONDOC
    */
  def onValue (slot :Consumer[T]) :Connection = onValueAt(0)(slot)

  /** Connects the supplied slot (side-effecting function) at the specified priority. When a value is
    * emitted, the slot will be invoked with the value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onValueAt (prio :Int)(slot :Consumer[T]) :Connection = addLink(new Link(this, prio) {
    override def notify (value :Any, ovalue :Any) = slot.accept(value.asInstanceOf[T])
  })

  /** Connects `slot` to this value with priority 0; it will be invoked when the value changes. Also
    * immediately invokes `slot` with the current value.
    * @return $CONDOC
    */
  def onValueNotify (slot :Consumer[T]) :Connection = onValueNotifyAt(0)(slot)

  /** Connects `slot` to this value; it will be invoked when the value changes. Also immediately
    * invokes `slot` with the current value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onValueNotifyAt (prio :Int)(slot :Consumer[T]) :Connection = {
    // connect before notifying the slot; if the slot changes the value during execution, it will
    // expect to be notified of that change; but if the slot throws an exception, we need to take
    // care of disconnecting because the returned connection will never reach the caller
    val conn = onValueAt(prio)(slot)
    try {
      slot.accept(getForNotify)
      conn
    } catch {
      case e :Throwable => conn.close(); throw e
    }
  }

  /** Connects the supplied slot (side-effecting function) with priorty zero. When a value is
    * emitted, the slot will be invoked with the value.
    * @return $CONDOC
    */
  def onChange (slot :BiConsumer[T,T]) :Connection = onChangeAt(0)(slot)

  /** Connects the supplied slot (side-effecting function) at the specified priority. When a value is
    * emitted, the slot will be invoked with the value.
    * @param prio $PRIODOC
    * @return $CONDOC
    */
  def onChangeAt (prio :Int)(slot :BiConsumer[T,T]) :Connection = addLink(new Link(this, prio) {
    override def notify (value :Any, ovalue :Any) =
      slot.accept(value.asInstanceOf[T], ovalue.asInstanceOf[T])
  })

  /** Returns the current value, for use by [[onValueNotifyAt]]. */
  protected def getForNotify :T

  /** Emits a changed value. Default implementation immediately notifies listeners. */
  protected def emitChange (value :T, ovalue :T) = notifyEmit(value, ovalue)

  /** Emits the supplied value to all connections. */
  protected def notifyEmit (value :T, ovalue :T) :Unit = {
    val lners = prepareNotify()
    var err :ReactionException = null
    try {
      var cons = lners
      while (cons != null) {
        try {
          cons.notify(value, ovalue)
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
