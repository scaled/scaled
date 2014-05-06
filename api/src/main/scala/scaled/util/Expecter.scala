//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.{BufferedReader, File, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.util.concurrent.Executor

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

  /** Sends a line of text to the command's stdin. A newline is automatically appended. */
  def send (line :String) {
    assertProcess
    out.println(line)
  }

  /** Terminates the subprocess forcibly. */
  def kill () {
    if (process != null) process.destroyForcibly()
  }

  /** Waits for the process to complete and returns its exit code. */
  def waitFor () :Int = {
    assertProcess
    process.waitFor()
  }

  /** Called when a line of text is received from the process's stdout. */
  def onOut (line :String) :Unit

  /** Called when a line of text is received from the process's stderr.
    * Defaults to calling [[onOut]] with the line. */
  def onErr (line :String) :Unit = onOut(line)

  /** Called if communication with the process fails. */
  def onFailure (exn :Exception) :Unit

  /** Returns the cwd to use when invoking the process. Override to customize. */
  protected def cwd :File = new File(System.getProperty("user.dir"))

  /** An array of environment variables to be supplied to the new process. These are of the
    * form `name=value`. Defaults to none. Override to customize. */
  protected def env :Array[String] = Array[String]()

  private val process = try {
    Runtime.getRuntime.exec(command.toArray, env, cwd)
  } catch {
    case e :Exception => onFailure(e) ; null
  }
  if (process != null) {
    new Thread() {
      setDaemon(true)
      override def run () = read(in, onOut)
    }.start()
    new Thread() {
      setDaemon(true)
      override def run () = read(err, onErr)
    }.start()
  }

  private lazy val out = new PrintWriter(new OutputStreamWriter(process.getOutputStream, "UTF-8"))
  private lazy val in = new BufferedReader(new InputStreamReader(process.getInputStream, "UTF-8"))
  private lazy val err = new BufferedReader(new InputStreamReader(process.getErrorStream, "UTF-8"))

  private def assertProcess :Unit =
    if (process == null) throw new IllegalStateException("Process failed to start")

  private def read (reader :BufferedReader, onLine :String => Unit) :Unit = try {
    while (true) {
      val line = reader.readLine
      if (line == null) return
      exec.execute(new Runnable() { override def run () = onLine(line) })
    }
  } catch {
    case e :Exception => exec.execute(new Runnable() { override def run () = onFailure(e) })
  }
}
