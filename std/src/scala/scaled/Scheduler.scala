//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.concurrent.Executor

/**
 * Extends the [[Executor]] API with delayed scheduling.
 */
trait Scheduler extends Executor {

  /** Schedules `op` to run as soon as possible. */
  def execute (op :Runnable) :Unit

  /** Schedules `op` to run after `delay` milliseconds have elapsed.
    * @return a [Closeable] via which to cancel the operation (if it has not already executed). */
  def schedule (delay :Long, op :Runnable) :Closeable

  // TODO: do we want schedulePeriodically?
}
