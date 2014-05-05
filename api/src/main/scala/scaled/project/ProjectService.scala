//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scaled.Service

/** Provides "project" services. A project is, at its simplest, a directory tree that contains a
  * bunch of related files. A sentinel file or directory usually indicates the root of the project
  * tree. For example:
  *
  *  - a `pom.xml` file for Maven projects
  *  - a `build.sbt` file (or `project/Foo.scala` file) for SBT projects
  *  - a `.git` directory for unknown projects (which can at least provide some simple services)
  *
  * Project types that are "well supported" can provide many sophisticated services for files
  * in that project, including interfacing with a compiler or build tool to provide code
  * intelligence, opening files in a project, searching across files in a project, displaying
  * documentation for code in a project, and in some cases also handling these things for
  * projects on which the current project depends.
  *
  * The project manager (which provides `ProjectService`) resolves projects on demand the first
  * time they are referenced, and then keeps them around while buffers operating on files in the
  * project remain open.
  */
@Service(name="project", impl="ProjectManager", desc="""
  Provides 'project intelligence' to modes, which ranges from simple things like knowing all the
  files that are in a project to more sophisticated things like interfacing with a compiler to
  provide code completion, etc.""")
trait ProjectService {

  /** Resolves (if necessary) and returns the project which handles `file`.
    * This may be [[DefaultProject]] if no more sophisticated project can be determined.
    */
  def projectFor (file :File) :Project

  /** Returns the project with id `id` if such a project is known. */
  def projectForId (id :String) :Option[Project]

  /** Returns the project with source URL `srcURL if such a project is known. */
  def projectForSrcURL (srcURL :String) :Option[Project]

  /** Returns all currently resolved projects. */
  def loadedProjects :Seq[Project]

  /** Returns `(root, name)` for all known projects (which may or may not be resolved). */
  def knownProjects :Seq[(File,String)]
}
