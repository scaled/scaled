//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scaled._

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager extends AbstractService with ProjectService {

  def didStartup () {
    // TODO!
  }

  def willShutdown () {
    // TODO!
  }

  def projectFor (file :File) = {
    // TODO: check whether an open project already handles this file

    DefaultProject
  }
}

object DefaultProject extends Project {

  override val fileCompleter = Completer.file
}
