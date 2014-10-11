//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/** Provides a read-only view of a property. This is useful if you want to expose reading of a
  * [[ValueV]] without providing the ability to register reactions.
  */
trait PropertyV[T] {

  /** Returns the current value of this property. */
  def get :T

  /** Returns the current value of this property. This is a synonym for [[get]] so that one can use
    * Scala's special apply syntax (e.g. `myprop()` instead of `myprop.get`). */
  def apply () :T
}
