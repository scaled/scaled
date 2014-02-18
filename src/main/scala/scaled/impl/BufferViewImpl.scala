//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import reactual.{Future, Value}

import scaled._

/** Implements [[BufferView]] and [[RBufferView]]. This class mainly defines the model, and
  * [[CodeArea]] etc. actually visualize the model and handle UX.
  */
class BufferViewImpl (_buffer :BufferImpl) extends RBufferView {

  override def buffer = _buffer
  override def lines = Seq() // TODO

  override def minibufferRead (prompt :String, defval :String) =
    Future.failure(new Exception("TODO"))
  // TODO: minibufferRead variant that takes a tab-completer? mode provides?

  override def emitStatus (msg :String) = println(s"STATUS: $msg")
}
