//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/**
 * Provides a mechanism to cancel a slot or listener registration, or to perform post-registration
 * adjustment like making the registration single-shot.
 */
trait Connection extends java.io.Closeable {

  /**
   * Disconnects this registration. Subsequent events will not be dispatched to the associated slot
   * or listener.
   */
  override def close () :Unit

  /**
   * Converts this connection into a one-shot connection. After the first time the slot or listener
   * is notified, it will automatically be disconnected.
   * @return this connection instance for convenient chaining.
   */
  def once () :Connection
}

object Connection {

  /** A connection that does nothing. */
  val Noop :Connection = new Connection() {
    override def close () {}
    override def once () = this
  }
}
