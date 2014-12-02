//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.net.{DatagramPacket, DatagramSocket}

/** Hosts a simple server on localhost:[[Port]]. Currently accepts only a single command:
  * `open PATH`
  * which causes `PATH` to be opened in the most recently used editor on the currently active
  * desktop (if Scaled can figure out what desktop is active).
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
        if (cmd startsWith "open ") onMainThread { app.wspMgr.visit(cmd.substring(5).trim) }
        else app.logger.log(s"Unknown command: '$cmd'")
      }

    } catch {
      case e :Exception => app.logger.log(s"Failed to bind to $Port", e)
    }
  }
}
