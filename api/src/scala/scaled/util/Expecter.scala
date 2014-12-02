//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

/** Manages an interaction with a separate process, via its stdin, stdout and stderr.
  * This is something sufficiently common for modes and services to do that Scaled provides this
  * built-in utility for simplifying the process.
  *
  * When an expecter instance is constructed, the associated command is immediately spawned and
  * interaction begins.
  *
  * @param exec needed to route callbacks to the UI thread.
  * @param config the subprocess configuration.
  */
abstract class Expecter (exec :Executor, config :SubProcess.Config) {
  import SubProcess._

  private val events = Signal[SubProcess.Event](exec.uiExec)
  events.onValue { _ match {
    case Output(text, isErr) =>
      if (_responder == null) onUnexpected(text, isErr)
      else if (_responder(text, isErr)) _responder = null
    case Failure(cause, isErr) =>
      onFailure(cause, isErr)
    case Complete(_) => // nada
  }}
  private val proc = new SubProcess(config, events)

  /** Initiates an interaction with the subprocess. An interaction consists of sending one or more
    * lines of text to the subprocess and then piping subprocess output to a supplied responder
    * until the responder indicates that the interaction is complete.
    *
    * @param responder a function that takes (line, isStdErr) and returns true if the interaction
    * is complete, false otherwise. This will be called on the UI thread.
    *
    * @return true if the interaction was started, false if it was not because another interaction
    * is still active.
    */
  def interact (input :Seq[String])(responder :(String, Boolean) => Boolean) :Boolean = {
    if (_responder != null) false
    else {
      _responder = responder
      proc.send(input)
      true
    }
  }

  /** Called when output is received from the subprocess, but we have no configured responder. */
  def onUnexpected (line :String, isErr :Boolean) :Unit

  /** Called when an error occurs starting or reading from the subprocess. */
  def onFailure (exn :Throwable, isErr :Boolean) {
    onUnexpected(Errors.stackTraceToString(exn), isErr)
  }

  // delegation!
  def close () :Unit = proc.close()
  def kill () :Unit = proc.kill()
  def waitFor () :Int = proc.waitFor()

  private var _responder :(String, Boolean) => Boolean = _
}

object Expecter {

  /** Returns an expecter that logs unexpected output to `log`, prefixed with `ident`. */
  def withLogger (exec :Executor, log :Logger, prefix :String, command :String*) :Expecter =
    new Expecter(exec, SubProcess.Config(command.toArray)) {
      private def msg (msg :String, isErr :Boolean) = {
        val kind = if (isErr) "stderr" else "stdout"
        s"$prefix: [$kind] $msg"
      }
      override def onUnexpected (line :String, isErr :Boolean) {
        log.log(msg(line, isErr))
      }
      override def onFailure (exn :Throwable, isErr :Boolean) {
        exec.runOnUI { log.log(msg("expect failure", isErr), exn) }
      }
    }
}
