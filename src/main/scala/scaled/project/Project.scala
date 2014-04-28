//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scaled._

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project {

  /** Notes that buffer is now using this project. */
  def reference (buffer :Buffer) :this.type = {
    // TODO
    this
  }

  /** Notes that buffer is no longer using this project. */
  def release (buffer :Buffer) {
    // TODO
  }

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Completes files in this project. The string representation of the files should not be
    * prefixed with path information, but rather suffixed and only where necessary to avoid
    * name collisions.
    *
    * Thus one might see as possible completions: `Bar.scala Baz.scala(util/) Baz.scala(data/)`
    * When completing on `Ba`.
    */
  val fileCompleter :Completer[File]

  /** Returns the name of this project. */
  def name :String

  /** Returns the root of this project. */
  def root :File
}
