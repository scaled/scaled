//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.util.ArrayDeque
import java.util.concurrent.{Callable, Executor, ExecutionException, FutureTask}
import scaled._

/** Implements the machinery of a concurrent sequential process. A queue of actions is maintained
  * for each process and when a process has actions to be executed, it queues itself up on an
  * executor and executes all pending actions on the executor's thread.
  */
class Plumbing[P] (exec :Executor, process : => P) extends Pipe.Impl[P] with Runnable {

  override def execute (op :Runnable) = synchronized {
    _ops.offer(op)
    val wasActive = _active
    _active = true
    if (!wasActive) exec.execute(this)
  }

  override def tell (op :P => Unit) :Unit = execute(new Runnable() {
    override def run () { op(_process) }
  })

  override def ask[R] (f :P => R) = {
    if (Pipe.current == this) f(_process)
    else {
        val task = new FutureTask[R](new Callable[R]() {
            override def call = f(_process)
        })
        execute(task)
        // TODO: allow the timeout to be specified?
        try task.get
        // unwrap exceptions wrapped by Java due to checkedness
        catch { case ee :ExecutionException => throw ee.getCause }
    }
  }

  override def req[R] (f :P => R) = {
    val p = Promise[R]()
    execute(new Runnable() {
      override def run () = try p succeed f(_process)
                            catch { case t :Throwable => p fail t }
    })
    p
  }

  override def funnel [V] (f :V => Unit) = new Funnel[V]() {
    override def apply (value :V) = execute(new Runnable() {
      override def run () = f(value)
    })
  }

  override def run () {
    Pipe.curpipe.set(this)
    var op = pop()
    while (op != null) {
      try op.run()
      catch { case t :Throwable => reportError(t) }
      op = pop()
    }
    Pipe.curpipe.set(null)
  }

  /** Logs an error that happens during async action invocation. */
  protected def reportError (t :Throwable) {
    t.printStackTrace(System.err);
  }

  private[this] def pop () = synchronized {
    val top = _ops.poll
    _active = (top != null)
    top
  }

  private lazy val _process :P = {
    val p = process
    if (p == null) throw new NullPointerException("Context given null process")
    p
  }

  // we only manipulate these in push() and pop(), which are synchronized
  private[this] var _active :Boolean = false
  private[this] val _ops = new ArrayDeque[Runnable]()
}
