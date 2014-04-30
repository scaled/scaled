//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.net.{DatagramPacket, DatagramSocket}
import scala.annotation.tailrec

/** Hosts a simple server on localhost:[[Port]]. Currently accepts only a single command:
  * `open [@workspace] PATH`
  * which causes `PATH` to be opened in the editor identified by `workspace`.
  *
  * The indent of `workspace` is for the shell script to (optionally) pass a unique identifier
  * indicating the virtual workspace in which the shell script is being invoked, so that Scaled
  * can open an editor window for that workspace and open `PATH` in that window.
  */
class Server (app :Main) extends Thread {
  setDaemon(true)

  /** The port on which we listen for commands. */
  final val Port = System.getProperty("scaled.port", "32323").toInt

  override def run () {
    try {
      val sock = new DatagramSocket(Port)
      val buffer = new Array[Byte](4096)
      println(s"Listening for commands on localhost:$Port")
      while (true) {
        val pkt = new DatagramPacket(buffer, buffer.length)
        sock.receive(pkt)
        val cmd = new String(pkt.getData, 0, pkt.getLength, "UTF-8").trim
        cmd match {
          case OpenRe(id, path) =>
            val workspace = if (id == null) "default" else id.trim.substring(1)
            onMainThread { app.openInWorkspace(path.trim, workspace) }
          case _ => println(s"Unknown command: '$cmd'")
        }
      }

    } catch {
      case e :Exception => println(s"Failed to bind to $Port") ; e.printStackTrace(System.err)
    }
  }

  private val OpenRe = """open (@\S+)? *(.*)""".r
}
