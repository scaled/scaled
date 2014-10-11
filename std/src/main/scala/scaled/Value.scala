//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** A container for a single value, which may be observed for changes. */
class Value[T] (init :T) extends ValueV[T] with Property[T] {

  /** Updates this instance with the supplied value. Registered listeners are notified only if the
    * value differs from the current value.
    *
    * @return the previous value contained by this instance.
    * @throws $EXNDOC
    */
  def update (value :T) :T = updateAndNotifyIf(value)

  /** Updates this instance with the supplied value. Registered listeners are notified regardless of
    * whether the new value is equal to the old value.
    *
    * @return the previous value contained by this instance.
    * @throws $EXNDOC
    */
  def updateForce (value :T) :T = updateAndNotify(value)

  /** Updates this instance by applying `fn` to the current value and calling [[update]] on the
    * result. This is useful for mutating case classes stored in values, enabling usage like the
    * following: `v.updateMap(_.copy(foo=bar))`.
    *
    * @return the previous value contained by this instance.
    * @throws $EXNDOC
    */
  def updateMap (fn :T => T) :T = update(fn(get))

  override def get :T = _value

  override protected def updateLocal (value :T) = {
     val oldValue = _value
     _value = value
    oldValue
  }

  private[this] var _value :T = init
}

/** Helper methods for values. */
object Value {

  /** Creates an instance with the specified starting value. */
  def apply[T] (init :T) = new Value[T](init)
}
