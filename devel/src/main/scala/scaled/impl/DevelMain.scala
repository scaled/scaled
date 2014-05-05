//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.application.Application

class DevelMain extends Main {

  override def log (msg :String) {
    super.log(msg)
    System.err.println(msg)
  }
  override def log (msg :String, exn :Throwable) {
    super.log(msg, exn)
    System.err.println(msg)
    exn.printStackTrace(System.err)
  }
}

object DevelMain {

  def main (args :Array[String]) {
    Application.launch(classOf[DevelMain], args :_*)
  }
}
