//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

import reactual.Future
import scaled.Editor

/** Error reporting utilities. */
object Error {

  /** An exception reported to provide error feedback rather than indicate catastrophic failure.
    * [[Editor.emitError]] will report such exceptions to the user but not dump their stack trace
    * for debugging.
    */
  class FeedbackException (msg :String) extends Exception(msg)

  /** Creates an exception to report the supplied error message. */
  def feedback (msg :String) = new FeedbackException(msg)

  /** Creates a [[Future]] which will fail with a the feedback message `msg`. */
  def futureFeedback[T] (msg :String) :Future[T] = Future.failure(feedback(msg))
}
