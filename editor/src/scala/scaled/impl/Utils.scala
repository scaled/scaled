//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import scaled._

/** Utilities used by the Scaled implementation code. */
object Utils {

  def safeSignal[T] (log :Logger) :Signal[T] = new Signal[T]() {
    override def emit (value :T) :Unit = try {
      super.emit(value)
    } catch {
      case t :Throwable => log.log(s"Signal.emit failure [value=$value]", t)
    }
  }
}
