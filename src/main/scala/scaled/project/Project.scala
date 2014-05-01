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

  /** Returns a unique identifier for this project, if one can be determined. Generally this is
    * Maven-style: `groupId:name:version`, but a `Project` is welcome to use whatever makes sense
    * for the kind of projects it manages.
    */
  def id :Option[String] = None

  /** Returns the version control source URL for this project, if one can be determined. This should
    * be prefixed with the version control type if it is not already naturally part of the URL:
    *
    * - git:https://github.com/scaled/maven-project.git
    * - git:git@github.com:samskivert/samskivert.git
    * - hg:https://ooo-maven.googlecode.com/hg/
    * - svn:https://ooo-gwt-utils.googlecode.com/svn
    */
  def sourceURL :Option[String] = None

  /** Notes that buffer is now using this project. */
  def reference (buffer :Buffer) :this.type = {
    _refcount += 1
    this
  }

  /** Notes that buffer is no longer using this project. */
  def release (buffer :Buffer) {
    assert(_refcount > 0, s"$this released with zero refcount!")
    _refcount -= 1
    if (_refcount == 0) hibernate()
  }

  // tracks the number of references to this project
  private[this] var _refcount = 0

  /** Returns a `Compiler` that can be used to recompile this project, if supported. */
  def compiler :Option[Compiler] = {
    if (_compiler == null) _compiler = createCompiler()
    _compiler
  }

  // a reference to our active compiler, if one is resolved; a compiler is created on demand
  // when a project mode references this project and requests it; the compiler remains active
  // until all project modes relinquish the project, at which point it's shutdown
  private[this] var _compiler :Option[Compiler] = null

  override def toString = s"Project($root, $name, $id, $sourceURL)"

  /** When a project is released by all project modes, it goes back into hibernation. This method
    * should shut down any complex services maintained by the project. The default implementation
    * shuts down any active compiler.
    */
  protected def hibernate () {
    if (_compiler != null) {
      _compiler foreach { _.shutdown() }
      _compiler = null
    }
  }

  /** If this mode supports a compiler, this should create and return a new compiler instance. */
  protected def createCompiler () :Option[Compiler] = None
}
