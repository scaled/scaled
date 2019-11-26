//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._
import scaled.util.{Errors, SubProcess}

@Minor(name="subproc", tags=Array("*"), stateTypes=Array(classOf[SubProcess]),
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
        message("(killing active subproc before starting new)")
        oproc.kill()
      }
    }
    // reset our C-g tracker
    termAttempted = false
  })

  @Fn("""Describes the subprocess bound to this buffer, if any.""")
  def describeSubprocess () :Unit = {
    message(requireProc.toString)
  }

  @Fn("""Sends SIGINT to the attached subprocess.""")
  def interruptSubprocess () :Unit = {
    requireProc.pid match {
      case None      => throw Errors.feedback("Unable to get subprocess pid. Not on Unix?")
      case Some(pid) => Runtime.getRuntime.exec("kill -INT " + pid).waitFor()
    }
  }

  @Fn("""Attempts to quit the attached subprocess by sending SIGTERM.
         If this function is invoked a second time and the subprocess is still alive,
         it will be terminated via SIGKILL.""")
  def killSubprocess () :Unit = {
    val proc = requireProc
    if (!proc.isAlive) throw Errors.feedback(NoActive)
    else if (termAttempted) {
      proc.kill()
      message("(sent SIGKILL to subproc)")
    }
    else {
      proc.terminate()
      message("(sent SIGTERM to subproc)")
      termAttempted = true
    }
  }

  private def NoActive = "No active subprocess in buffer."
  private def requireProc = buffer.state.req[SubProcess](NoActive)
  private def message (text :String) :Unit = buffer.append(Line.fromTextNL(text))
}
