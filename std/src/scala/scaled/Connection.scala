//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

/**
 * Provides a mechanism to cancel a slot or listener registration, or to perform post-registration
 * adjustment like making the registration single-shot.
 */
trait Connection extends Closeable {

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

  /** A connection that does nothing when closed. Simplifies situations where you close an old
    * connection and replace it with a new one. */
  val Noop :Connection = new Connection() {
    override def close () :Unit = {}
    override def once () = this
  }
}
