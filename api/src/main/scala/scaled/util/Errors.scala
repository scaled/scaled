//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

import java.io.{PrintWriter, StringWriter}
import reactual.Future
import scaled.Editor

/** Error reporting utilities. */
object Errors {

  /** An exception reported to provide error feedback rather than indicate catastrophic failure.
    * [[Editor.emitError]] will report such exceptions to the user but not dump their stack trace
    * for debugging.
    */
  class FeedbackException (msg :String) extends Exception(msg)

  /** Returns true if `t` is a feedback exception. */
  def isFeedback (t :Throwable) :Boolean = t.isInstanceOf[FeedbackException]

  /** Creates an exception to report the supplied error message. */
  def feedback (msg :String) = new FeedbackException(msg)

  /** Creates a [[Future]] which will fail with a the feedback message `msg`. */
  def futureFeedback[T] (msg :String) :Future[T] = Future.failure(feedback(msg))

  /** Converts `exn`'s stack trace into a string. */
  def stackTraceToString (exn :Throwable) :String = {
    val trace = new StringWriter()
    exn.printStackTrace(new PrintWriter(trace))
    trace.toString
  }
}
