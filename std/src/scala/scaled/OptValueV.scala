//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A view of an [[OptValue]] which may be observed, but not updated.
  */
abstract class OptValueV[T] extends ValueReactor[Option[T]] {

  /** Returns whether this value is non-empty. */
  def isDefined :Boolean

  /** Returns whether this value is empty. */
  def isEmpty :Boolean = !isDefined

  /** Returns the current value.
    * @throws NoSuchElementException if the value is currently empty. */
  def get :T

  /** Returns the current value. This is a synonym for [[get]] so that one can use Scala's special
    * apply syntax (e.g. `myval()` instead of `myval.get`).
    * @throws NoSuchElementException if the value is currently empty. */
  def apply () :T = get

  /** Returns the contents of this value as an option. */
  def getOption :Option[T] = if (isDefined) Some(get) else None

  /** Returns the current value if one is set, `alt` otherwise. */
  @inline final def getOrElse[B >: T] (alt : => B) :B = if (isDefined) get else alt

  /** Maps the contents of this value via `f`. When this value is updated, the mapped value will
    * emit that value as transformed by `f`. When this value is cleared, the mapped value is also
    * cleared. A call to `get` on the mapped value will call get on this value and transform the
    * result via `f` before returning it. The mapped value will retain a connection to this value
    * for as long as it has connections of its own.
    */
  def map[M] (f :T => M) :OptValueV[M] = {
    val outer = this
    new OptValueV[M]() {
      override def get = f(outer.get)
      override def isDefined = outer.isDefined
      // connectionAdded and connectionRemoved are only ever called with a lock held on this reactor,
      // so we're safe in checking and mutating _conn
      override protected def connectionAdded () {
        super.connectionAdded()
        if (_conn == null) _conn = outer.onChange((nv, ov) => notifyEmit(nv.map(f), ov.map(f)))
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

  /** Returns a signal which emits a value whenever `this` value changes. */
  def asSignal :SignalV[Option[T]] = {
    new SignalV[Option[T]] {
      // connectionAdded and connectionRemoved are only ever called with a lock held on this
      // reactor, so we're safe in checking and mutating _conn
      override protected def connectionAdded () {
        super.connectionAdded()
        if (_conn == null) _conn = OptValueV.this.onValue(notifyEmit)
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

  override def hashCode = if (isEmpty) 0 else get.hashCode

  override def equals (other :Any) = {
    if (other == null || other.getClass != getClass) false
    else {
      val ov = other.asInstanceOf[OptValueV[_]]
      (isEmpty && ov.isEmpty) || (get == ov.get)
    }
  }

  override def toString :String =
    if (isDefined) s"$shortClassName($get)" else s"$shortClassName(<empty>)"

  override protected def getForNotify = getOption

  /** Updates the value contained in this instance and notifies registered listeners iff said value
    * is not equal to the value already contained in this instance.
    */
  protected def updateAndNotifyIf (value :T) :Unit = updateAndNotify(value, false)

  /** Updates the value contained in this instance and notifies registered listeners.
    * @return the previously contained value.
    */
  protected def updateAndNotify (value :T) :Unit = updateAndNotify(value, true)

  /** Updates the value contained in this instance and notifies registered listeners.
    * @param force if true, the listeners will always be notified, if false the will be notified
    * only if the new value is not equal to the old value.
    * @return the previously contained value.
    */
  protected def updateAndNotify (value :T, force :Boolean) :Unit = {
    checkMutate()
    val ovalue = updateLocal(value)
    if (force || value != ovalue) emitChange(Option(value), Option(ovalue))
  }

  /** Updates our locally stored value. Default implementation throws unsupported operation.
    * @return the previously stored value.
    */
  protected def updateLocal (value :T) :T = throw new UnsupportedOperationException
}
