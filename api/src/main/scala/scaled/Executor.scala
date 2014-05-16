//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Handles invoking execution units on the UI thread and background threads. */
trait Executor {

  /** Invokes `op` on the next UI tick. This can be invoked from the UI thread to defer an operation
    * until the next tick, or it can be invoked from a background thread to process results back on
    * the UI thread.
    */
  def runOnUI (op :Runnable) :Unit

  /** Invokes `op` on a background thread. There are a pool of background threads, so multiple
    * invocations of this method may result in the operations being run in parallel. An operation
    * sent to a background thread should not manipulate any data visible to other threads. Use
    * [[runOnUI]] to send results back to the UI thread to display to the user.
    */
  def runInBackground (op :Runnable) :Unit

  /** A Scala-friendly [[runOnUI(Runnable)]]. */
  def runOnUI[U] (op : => U) :Unit = runOnUI(new Runnable() {
    override def run () = op
  })

  /** A Scala-friendly [[runInBackground(Runnable)]]. */
  def runInBackground (op : => Unit) :Unit = runInBackground(new Runnable() {
    override def run () = op
  })
}
