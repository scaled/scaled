//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.util.concurrent.{Executor => JExecutor}

/** Provides a handle via which one can communicate with a concurrent sequential process. A
  * concurrent sequential process is a collection of code and data which runs sequentially (multiple
  * threads are never executing code in the process at the same time) but which runs concurrently
  * with all other sequential processes. Note: this is more like actors than traditional CSP.
  *
  * The process itself is an object which should only be accessed via its pipe. Ideally nothing
  * retains a reference to the process object itself as that would allow methods to be invoked on
  * the process object from (potentially) the wrong thread. Only retain a reference to a process's
  * pipe, and there's no risk of doing things incorrectly.
  *
  * In Scaled, each [[Service]] is implemented by a process. Services may also spawn other processes
  * where appropriate and pass out their pipes.
  *
  * There are three ways to communicate with a process:
  *
  *  - [[tell]]ing it something: this is a fire-and-forget mechanism to invoke code on the
  *    process "eventually"
  *
  *  - [[ask]]ing it something: this sends a request to the process and blocks the caller until the
  *    result is received. This is the most common way to communicate with processes. Most requests
  *    can be satisfied immediately by the process, so the tiny overhead of coordinated inter-thread
  *    communication is no reason to weird your code with asynchronicity. Note: no special deadlock
  *    avoidance is performed, so avoid massive chains of processes asking things.
  *
  *  - [[req]]uesting something: this sends a request to the process and returns a [[Future]] via
  *    which the result is made available. If you're asking for something that might actually take
  *    a long time, then this is the appropriate mechanism.
  *
  * Pipe also implements `Executor` allowing runnables to be queued up for execution in the
  * process's threading context.
  *
  * @define SAMECTX `f` will never be invoked immediately, even if the calling thread is currently
  * in this entity's execution context.
  */
abstract class Pipe[E] extends JExecutor {
  import Pipe._

  /** Dispatches `f` on the target process (on the appropriate thread). $SAMECTX */
  def tell (f :E => Unit)

  /** Dispatches `f` on the target process (on the appropriate thread) and blocks the calling thread
    * until the response is available. If the calling thread is currently in the target process's
    * execution context, `f` will be invoked directly. However, this is a code smell. You should
    * know that you're in a process's execution context and simply call methods directly. */
  def ask[R] (f :E => R) :R

  /** Dispatches `f` on the target process (on the appropriate thread) and makes the result
    * available as a future. $SAMECTX */
  def req[R] (f :E => R) :Future[R]
}

/** "Funnels" values into a pipe. This facilitates type-checked callbacks between processes.
  * Process B can expose an API which accepts a funnel from the calling process, and process B
  * knows that it can deliver values to the funnel, which will result in some method being called
  * on the calling process in the calling process's execution context.
  */
abstract class Funnel[-V] extends (V => Unit) {

  /** Delivers a value to this funnel. */
  def apply (value :V) :Unit
}

/** Static [[Pipe]] things. */
object Pipe {

  /** Returns a reference to the pipe for the process executing on this thread, or null. */
  def current :Pipe[_] = curpipe.get

  /** Creates a "funnel" into the currently executing process's pipe. When a value is applied to the
    * funnel, `f` will be invoked on the value in the currently executing process's execution
    * context. */
  def funnel[V] (f :V => Unit) :Funnel[V] = curpipe.get.funnel(f)

  private[scaled] abstract class Impl[E] extends Pipe[E] {
    def funnel [V] (f :V => Unit) :Funnel[V]
  }

  private[scaled] val curpipe = new ThreadLocal[Impl[_]]()
}
