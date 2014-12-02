//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** An injectable service that can be used for debug logging.
  *
  * User directed feedback should be delivered via [[Editor.emitStatus]], but internal logging
  * which is mainly for developer or end-user debugging can be sent here. It will be sent to the
  * `*messages*` buffer and when Scaled is run in development mode, will also be logged to the
  * console.
  */
trait Logger {

  /** Records `msg` to the log. */
  def log (msg :String) :Unit

  /** Records `msg` and the stack trace for `exn` to the log. */
  def log (msg :String, exn :Throwable) :Unit
}
