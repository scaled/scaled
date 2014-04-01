//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.{Set => MSet}

/** Helper routines for working with the file system. */
object Filer {

  /** Ensures that `dir` exists and is a directory.
    * Terminates the editor with an error message on failure. */
  def requireDir (dir :File) :File = {
    if (dir.exists()) {
      if (dir.isDirectory()) dir
      else fail(s"${dir.getAbsolutePath} should be a directory but is not.")
    }
    else if (dir.mkdir()) dir
    else fail(s"Unable to create ${dir.getAbsolutePath}.")
  }

  /** Applies `op` to all subdirectories, subsubdirectories, etc of `root`. */
  def descendDirs (root :File)(op :File => Unit) {
    val seen = MSet[File]()
    def apply (dir :File) :Unit = if (seen.add(dir)) dir.listFiles foreach { f =>
      if (f.isDirectory) { op(f) ; apply(f) }
    }
    apply(root)
  }

  /** Applies `op` to all files in `root` and in subdirectories (and subsubdirectories) thereof. */
  def descendFiles (root :File)(op :File => Unit) {
    val seen = MSet[File]()
    def apply (dir :File) :Unit = if (seen.add(dir)) dir.listFiles foreach { f =>
      if (f.isDirectory) apply(f)
      else op(f)
    }
    apply(root)
  }

  private def fail (msg :String) :Nothing = {
    System.err.println(s"$msg Scaled cannot operate without this directory.")
    sys.exit(255)
  }
}
