//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A view of a [[Value]] which may be observed, but not updated.
  */
abstract class ValueV[T] extends ValueReactor[T] with PropertyV[T] {

  /** Returns the current value. */
  def get :T

  /** Returns the current value. This is a synonym for [[get]] so that one can use Scala's special
    * apply syntax (e.g. `myval()` instead of `myval.get`). */
  def apply () :T = get

  /** Maps the output of this value via `f`. When this value is updated, the mapped value will emit
    * that value as transformed by `f`. A call to `get` on the mapped value will call get on this
    * value and transform the result via `f` before returning it. The mapped value will retain a
    * connection to this value for as long as it has connections of its own.
    */
  def map[M] (f :JFunction[T, M]) :ValueV[M] = {
    val outer = this
    new ValueV[M]() {
      override def get = f(outer.get)
      // connectionAdded and connectionRemoved are only ever called with a lock held on this reactor,
      // so we're safe in checking and mutating _conn
      override protected def connectionAdded () {
        super.connectionAdded()
        if (_conn == null) _conn = outer.onChange((v,o) => notifyEmit(f(v), f(o)))
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

  override def hashCode = get match {
    case null => 0
    case v    => v.hashCode
  }

  override def equals (other :Any) = {
    if (other == null) false
    else if (other.getClass != getClass) false
    else get == other.asInstanceOf[ValueV[_]].get
  }

  override def toString :String = s"$shortClassName($get)"

  override protected def getForNotify = get

  /** Updates the value contained in this instance and notifies registered listeners iff said value
    * is not equal to the value already contained in this instance.
    */
  protected def updateAndNotifyIf (value :T) :T = updateAndNotify(value, false)

  /** Updates the value contained in this instance and notifies registered listeners.
    * @return the previously contained value.
    */
  protected def updateAndNotify (value :T) :T = updateAndNotify(value, true)

  /** Updates the value contained in this instance and notifies registered listeners.
    * @param force if true, the listeners will always be notified, if false the will be notified
    * only if the new value is not equal to the old value.
    * @return the previously contained value.
    */
  protected def updateAndNotify (value :T, force :Boolean) : T = {
    checkMutate()
    val ovalue = updateLocal(value)
    if (force || value != ovalue) emitChange(value, ovalue)
    ovalue
  }

  /** Updates our locally stored value. Default implementation throws unsupported operation.
    * @return the previously stored value.
    */
  protected def updateLocal (value :T) :T = throw new UnsupportedOperationException
}
