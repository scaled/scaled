//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** Models a read-write property. This is useful if you want to expose reading and writing of a
  * [[Value]] without providing the ability to register reactions.
  */
trait Property[T] extends PropertyV[T] {

  /** Updates the value of this property. */
  def update (value :T) :T
}
