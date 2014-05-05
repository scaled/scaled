//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File

import scala.collection.mutable.{Set => MSet}

/** Helper routines for working with the file system. */
object Filer {

  /** Composes `root` and `comps` into a new file representing the final path component. */
  def file (root :File, comps :String*) :File = (root /: comps)(new File(_, _))

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

  /** Applies `op` to all subdirectories, subsubdirectories, etc of `root`. If `op` returns false, we
    * descend into the directory, if it returns true we do not. */
  def descendDirs (root :File)(op :File => Boolean) {
    val seen = MSet[File]()
    def apply (dir :File) :Unit = if (seen.add(dir)) dir.listFiles foreach { f =>
      if (f.isDirectory) { if (!op(f)) apply(f) }
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

  /** Returns true if this file represents an entry in a `zip` or `jar` archive. It will have a
    * path of the form `foo/bar.jar!entryname.suff`.
    */
  def isArchiveEntry (file :File) :Boolean = {
    val path = file.getPath
    path.contains(".zip!") || path.contains(".jar!")
  }

  private def fail (msg :String) :Nothing = {
    System.err.println(s"$msg Scaled cannot operate without this directory.")
    sys.exit(255)
  }
}
