//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.concurrent.{Executor => JExecutor}
import reactual.{Future, Promise}

/** Handles invoking execution units on the UI thread and background threads. */
abstract class Executor {

  /** A Java `Executor` which runs operations on the UI thread. */
  val uiExec :JExecutor

  /** A Java `Executor` which runs operations on a background thread pool. */
  val bgExec :JExecutor

  /** Invokes `op` on the next UI tick. This can be invoked from the UI thread to defer an operation
    * until the next tick, or it can be invoked from a background thread to process results back on
    * the UI thread.
    */
  def runOnUI (op :Runnable) :Unit = uiExec.execute(op)

  /** Invokes `op` on a background thread. There are a pool of background threads, so multiple
    * invocations of this method may result in the operations being run in parallel. An operation
    * sent to a background thread should not manipulate any data visible to other threads. Use
    * [[runOnUI]] to send results back to the UI thread to display to the user.
    */
  def runInBG (op :Runnable) :Unit = bgExec.execute(op)

  /** A Scala-friendly [[runOnUI(Runnable)]]. */
  def runOnUI[U] (op : => U) :Unit = uiExec.execute(new Runnable() {
    override def run () = op
  })

  /** A Scala-friendly [[runInBG(Runnable)]]. */
  def runInBG (op : => Unit) :Unit = bgExec.execute(new Runnable() {
    override def run () = op
  })

  /** Runs `op` on a background thread, reports the results on the UI thread.
    * @return a future that will deliver the result or cause of failure when available. */
  def runAsync[R] (op : => R) :Future[R] = {
    val result = uiPromise[R]()
    bgExec.execute(new Runnable() {
      override def run () = try result.succeed(op) catch { case t :Throwable => result.fail(t) }
    })
    result
  }

  /** Returns a promise which will notify listeners of success or failure on the UI thread,
    * regardless of the thread on which it is completed. */
  def uiPromise[R] () :Promise[R] = new Promise[R]() {
    private def superSucceed (value :R) = super.succeed(value)
    override def succeed (value :R) = runOnUI(new Runnable() {
      def run () = superSucceed(value)
    })
    private def superFail (cause :Throwable) = super.fail(cause)
    override def fail (cause :Throwable) = runOnUI(new Runnable() {
      def run () = superFail(cause)
    })
  }

  /** Returns a future which completes on the UI thread after `delay` milliseconds. Completion
    * callbacks can be registered on this future to invoke code on the UI thread after the delay.
    * Using futures to model timers allows us to avoid reimplmenting callbacks and cancelations for
    * yet another time. */
  def uiTimer (delay :Long) :Future[Unit] = Future.failure(new UnsupportedOperationException())
}
