//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.net.{DatagramPacket, DatagramSocket}
import scala.annotation.tailrec

/** Hosts a simple server on localhost:[[Port]]. Currently accepts only a single command:
  * `open [@desktop] PATH`
  * which causes `PATH` to be opened in the editor identified by `desktop`.
  *
  * The indent of `desktop` is for the shell script to (optionally) pass a unique identifier
  * indicating the virtual desktop in which the shell script is being invoked, so that Scaled can
  * open an editor window for that desktop and open `PATH` in that window.
  */
class Server (app :Scaled) extends Thread {
  setDaemon(true)

  /** The port on which we listen for commands. */
  final val Port = System.getProperty("scaled.port", "32323").toInt

  override def run () {
    try {
      val sock = new DatagramSocket(Port)
      val buffer = new Array[Byte](4096)
      app.logger.log(s"Listening for commands on localhost:$Port")
      while (true) {
        val pkt = new DatagramPacket(buffer, buffer.length)
        sock.receive(pkt)
        val cmd = new String(pkt.getData, 0, pkt.getLength, "UTF-8").trim
        cmd match {
          case OpenRe(id, path) =>
            val desktop = if (id == null) "default" else id.trim.substring(1)
            onMainThread { app.openInDesktop(path.trim, desktop) }
          case _ => app.logger.log(s"Unknown command: '$cmd'")
        }
      }

    } catch {
      case e :Exception => app.logger.log(s"Failed to bind to $Port", e)
    }
  }

  private val OpenRe = """open (@\S+)? *(.*)""".r
}
