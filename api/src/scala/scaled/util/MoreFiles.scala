//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.nio.file.FileVisitResult
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.util.function.Consumer
import scaled._

/** Provides things that we wish `Files` provided for us. */
object MoreFiles {

  /** Applies `op` to all files in the directory trees rooted at `dirs`. */
  def onFiles (dirs :SeqV[Path], op :Consumer[Path]) :Unit =
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op.accept(file)
          FileVisitResult.CONTINUE
        }
      })
    }

  /** Applies `op` to all files in the directory trees rooted at `dirs`.
    * @param dirFilter a function called on each directory prior to enumerating its contents,
    * return `true` to include the directory, `false` to omit it. */
  def onFilteredFiles (dirs :SeqV[Path], dirFilter :Path => Boolean, op :Consumer[Path]) :Unit =
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op.accept(file)
          FileVisitResult.CONTINUE
        }
        override def preVisitDirectory (dir :Path, attrs :BasicFileAttributes) =
          if (dirFilter(dir)) FileVisitResult.CONTINUE else FileVisitResult.SKIP_SUBTREE
      })
    }
}
