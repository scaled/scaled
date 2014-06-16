//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.{Files, Path}
import java.util.function.Consumer
import scala.collection.mutable.{Set => MSet}

/** Helper routines for working with the file system. */
object Filer {

  /** Ensures that `dir` exists and is a directory.
    * Terminates the editor with an error message on failure. */
  def requireDir (dir :Path) :Path = {
    if (!Files.exists(dir)) Files.createDirectory(dir)
    else if (Files.isDirectory(dir)) dir
    else fail(s"$dir should be a directory but is not.")
  }

  /** Applies `op` to all subdirectories, subsubdirectories, etc of `root`. If `op` returns false,
    * we descend into the directory, if it returns true we do not. */
  def descendDirs (root :Path)(op :Path => Boolean) {
    val seen = MSet[Path]()
    def apply (dir :Path) :Unit = if (seen.add(dir)) Files.list(dir).forEach(new Consumer[Path] {
      def accept (p :Path) = if (Files.isDirectory(p)) { if (!op(p)) apply(p) }
    })
    apply(root)
  }

  /** Applies `op` to all files in `root` and in subdirectories (and subsubdirectories) thereof. */
  def descendFiles (root :Path)(op :Path => Unit) {
    val seen = MSet[Path]()
    def apply (dir :Path) :Unit = if (seen.add(dir)) Files.list(dir).forEach(new Consumer[Path] {
      def accept (p :Path) = if (Files.isDirectory(p)) apply(p)
                             else op(p)
    })
    apply(root)
  }

  private def fail (msg :String) :Nothing = {
    System.err.println(s"$msg Scaled cannot operate without this directory.")
    sys.exit(255)
  }
}
