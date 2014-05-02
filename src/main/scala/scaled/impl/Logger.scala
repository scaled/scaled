//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

/** Used for internal logging. */
trait Logger {

  /** Records `msg` to the log. */
  def log (msg :String) :Unit

  /** Records `msg` and the stack trace for `exn` to the log. */
  def log (msg :String, exn :Throwable) :Unit
}
