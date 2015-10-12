//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._
import scaled.util.{Errors, SubProcess}

@Minor(name="subproc", stateTypes=Array(classOf[SubProcess]),
       desc="""Provides a means to interact with a sub-process bound to a buffer.""")
class SubProcessMode (env :Env) extends MinorMode(env) {

  private var termAttempted = false

  // terminate our subprocess if the buffer is killed
  note(buffer.killed.onEmit {
    buffer.state.get[SubProcess] foreach { _.terminate() }
  })

  override def keymap = super.keymap.
    bind("interrupt-subprocess", "C-c C-c").
    bind("kill-subprocess", "C-g");

  // listen for changes to the SubProcess state of the buffer
  buffer.state[SubProcess].onChange((nprocO, oprocO) => {
    // if a new subprocess is set in this buffer, kill the old one
    oprocO foreach { oproc =>
      if (oproc.isAlive) {
        // TODO: report to the buffer?
        oproc.kill()
      }
    }
    // reset our C-g tracker
    termAttempted = false
  })

  @Fn("""Describes the subprocess bound to this buffer, if any.""")
  def describeSubprocess () {
    val proc = buffer.state.req[SubProcess]("No active subprocess in buffer.")
    buffer.append(Line.fromText(proc.toString))
  }

  @Fn("""Sends SIGINT to the attached subprocess.""")
  def interruptSubprocess () {
    val proc = buffer.state.req[SubProcess]("No active subprocess in buffer.")
    proc.pid match {
      case None      => throw Errors.feedback("Unable to get subprocess pid. Not on Unix?")
      case Some(pid) => Runtime.getRuntime.exec("kill " + pid).waitFor()
    }
  }

  @Fn("""Attempts to quit the attached subprocess by sending SIGTERM.
         If this function is invoked a second time and the subprocess is still alive,
         it will be terminated via SIGKILL.""")
  def killSubprocess () {
    val proc = buffer.state.req[SubProcess]("No active subprocess in buffer.")
    if (termAttempted) proc.kill()
    else {
      proc.terminate()
      termAttempted = true
    }
  }
}
