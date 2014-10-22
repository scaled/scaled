//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.nio.file.{Path, Paths}
import scaled._

/** Factory methods &c for [[SubProcess]. */
object SubProcess {

  /** Configures a to-be-created subprocess. */
  case class Config (
    /** The command and arguments to be executed. */
    cmd :Array[String],
    /** The environment in which to invoke the command. Defaults to empty. */
    env :Map[String,String] = Map(),
    /** The current working directory for the process. Defaults to Scaled's cwd. */
    cwd :Path = Paths.get(System.getProperty("user.dir"))
  )

  /** Starts a subprocess with the specified configuration. Output will be directed to `buffer`. If
    * the subprocess fails to start, the starting exception will be captured, recorded to `buffer`
    * and then rethrown. */
  def apply (config :Config, exec :Executor, buffer :Buffer) :SubProcess = try {
    new SubProcess(config) {
      protected def onOutput (text :String, isErr :Boolean) {
        exec.runOnUI(buffer.append(Line.fromTextNL(text)))
      }
    }
  } catch {
    case e :Throwable => buffer.append(Line.fromTextNL(Errors.stackTraceToString(e))) ; throw e
  }
}

/** Manages a sub-process. Routes output into a Scaled buffer and supports sending input to the
  * process if desired. This class is designed to be extended and configured by overriding the
  * myriad configuration methods. The process is started immediately upon instantiation of this
  * instance.
  */
abstract class SubProcess (config :SubProcess.Config) extends AutoCloseable {
  import SubProcess._

  /** Sends a line of text to the subprocess's stdin. A newline is automatically appended. This must
    * only be called after [[start]]. */
  def send (line :String) {
    out.println(line)
    out.flush()
  }

  /** Closes this subprocess's output stream. This may trigger termination if it expects that sort
    * of thing. This must only be called after [[start]]. */
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

  override def toString = s"SubProcess(${config.cmd.mkString(" ")})"

  /** Called when output arrives from stdout or stderr. In the latter case `isErr` will be true.
    * NOTE: this is called on the input reader threads. You must get yourself to the main editor
    * thread if you indend to do something like append the text to a buffer.
    */
  protected def onOutput (text :String, isErr :Boolean) :Unit

  /** Called when a failure is encountered by the stdout/stderr reader threads. The default
    * implementation formats the exception stack trace and passes it to [[onOutput]]. */
  protected def onFailure (exn :Exception, isErr :Boolean) {
    onOutput(Errors.stackTraceToString(exn), isErr)
  }

  private def read (reader :BufferedReader, isErr :Boolean) :Unit = try {
    while (true) {
      val line = reader.readLine
      if (line == null) return
      onOutput(line, isErr)
    }
  } catch {
    case e :Exception => onFailure(e, isErr)
  }

  private lazy val process = {
    val pb = new ProcessBuilder(config.cmd :_*)
    pb.directory(config.cwd.toFile)
    config.env foreach { (k, v) => pb.environment.put(k, v) }
    pb.start
  }

  protected lazy val out = new PrintWriter(new OutputStreamWriter(process.getOutputStream, "UTF-8"))

  // kick things off immediately; if the process fails to start, an exception will be thrown before
  // these threads are started; otherwise they'll run until the process's streams are closed
  { val in  = new BufferedReader(new InputStreamReader(process.getInputStream, "UTF-8"))
    val err = new BufferedReader(new InputStreamReader(process.getErrorStream, "UTF-8"))
    new Thread("Subproc: stdin") {
      setDaemon(true)
      override def run () = read(in, false)
    }.start()
    new Thread("Subproc: stderr") {
      setDaemon(true)
      override def run () = read(err, true)
    }.start()
  }
}
