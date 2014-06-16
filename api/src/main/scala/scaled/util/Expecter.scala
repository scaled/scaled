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
  * @param exec the executor via which callbacks are invoked on the UI thread.
  * @param config the subprocess configuration.
  */
abstract class Expecter (exec :Executor, config :SubProcess.Config) extends SubProcess(config) {

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
      input foreach out.println
      out.flush()
      true
    }
  }

  /** Called when output is received from the subprocess, but we have no configured responder. */
  def onUnexpected (line :String, isErr :Boolean) :Unit

  override protected def onOutput (text :String, isErr :Boolean) {
    exec.runOnUI {
      if (_responder == null) onUnexpected(text, isErr)
      else if (_responder(text, isErr)) _responder = null
    }
  }

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
      override protected def onFailure (exn :Exception, isErr :Boolean) {
        exec.runOnUI { log.log(msg("expect failure", isErr), exn) }
      }
    }
}
