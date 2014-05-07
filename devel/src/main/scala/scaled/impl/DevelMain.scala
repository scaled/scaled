//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.application.Application

class DevelMain extends Main {

  // send log messages to the console
  log onValue System.err.println
}

object DevelMain {

  def main (args :Array[String]) {
    Application.launch(classOf[DevelMain], args :_*)
  }
}
