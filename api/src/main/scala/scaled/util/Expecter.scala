//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.{BufferedReader, File, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.util.concurrent.Executor
import scaled.Editor

/** Manages an interaction with a separate process, via its stdin, stdout and stderr.
  * This is something sufficiently common for modes and services to do that Scaled provides this
  * built-in utility for simplifying the process.
  *
  * When an expecter instance is constructed, the associated command is immediately spawned and
  * interaction begins.
  *
  * @param exec the executor on which callbacks are invoked (usually pass in `editor`).
  * @param command the command to invoke and arguments passed thereto.
  */
abstract class Expecter (exec :Executor, command :String*) {

  /** Sends a line of text to the subprocess's stdin. A newline is automatically appended. */
  def send (line :String) {
    out.println(line)
    out.flush()
  }

  /** Initiates an interaction with the subprocess. An interaction consists of sending one or more
    * lines of text to the subprocess and then piping subprocess output to a supplied responder
    * until the responder indicates that the interaction is complete.
    *
    * @param responder a function that takes (line, isStdErr) and returns true if the interaction
    * is complete, false otherwise.
    *
    * @return true if the interaction was started, false if it was not because another interaction
    * is still active.
    */
  def interact (input :Seq[String], responder :(String, Boolean) => Boolean) :Boolean = {
    if (_responder != null) false
    else {
      _responder = responder
      input foreach out.println
      out.flush()
      true
    }
  }

  /** Closes this expecter's output stream. This may trigger termination for your subprocess if it
    * expects that sort of thing.
    */
  def close () {
    process.getOutputStream.close()
  }

  /** Terminates the subprocess forcibly. */
  def kill () {
    process.destroyForcibly()
  }

  /** Waits for the process to complete and returns its exit code. */
  def waitFor () :Int = {
    process.waitFor()
  }

  /** Called when output is received from the subprocess, but we have no configured responder. */
  def onUnexpected (line :String, isErr :Boolean) :Unit

  /** Called if communication with the process fails. */
  def onFailure (exn :Exception) :Unit

  override def toString = s"Expecter(${command.mkString(" ")})"

  /** Returns the cwd to use when invoking the process. Override to customize. */
  protected def cwd :File = new File(System.getProperty("user.dir"))

  /** An array of environment variables to be supplied to the new process. These are of the
    * form `name=value`. Defaults to none. Override to customize. */
  protected def env :Array[String] = Array[String]()

  private val process = Runtime.getRuntime.exec(command.toArray, env, cwd)

  // if the process fails to start, an exception will be thrown before these threads are started;
  // otherwise they'll run until the process's streams are closed
  new Thread("Expecter: stdin") {
    setDaemon(true)
    override def run () = read(in, false)
  }.start()
  new Thread("Expecter: stderr") {
    setDaemon(true)
    override def run () = read(err, true)
  }.start()

  private lazy val out = new PrintWriter(new OutputStreamWriter(process.getOutputStream, "UTF-8"))
  private lazy val in = new BufferedReader(new InputStreamReader(process.getInputStream, "UTF-8"))
  private lazy val err = new BufferedReader(new InputStreamReader(process.getErrorStream, "UTF-8"))

  private var _responder :(String, Boolean) => Boolean = _

  private def assertProcess :Unit =
    if (process == null) throw new IllegalStateException("Process failed to start")

  private def read (reader :BufferedReader, isErr :Boolean) :Unit = try {
    while (true) {
      val line = reader.readLine
      if (line == null) return
      exec.execute(new Runnable() { override def run () = handleInput(line, isErr) })
    }
  } catch {
    case e :Exception => exec.execute(new Runnable() { override def run () = onFailure(e) })
  }

  private def handleInput (line :String, isErr :Boolean) {
    if (_responder == null) onUnexpected(line, isErr)
    else if (_responder(line, isErr)) _responder = null
  }
}

object Expecter {

  /** Returns an expecter that logs output received outside interactions to editor,
    * prefixed with `ident`.
    */
  def inEditor (editor :Editor, prefix :String) :Expecter = new Expecter(editor) {
    def onUnexpected (line :String, isErr :Boolean) {
      val kind = if (isErr) "stderr" else "stdout"
      editor.log(s"$prefix: [$kind] $line")
    }
    def onFailure (err :Exception) {
      editor.log(s"$prefix: expect failure", err)
    }
  }
}
