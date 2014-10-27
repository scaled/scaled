//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.InputStream
import java.nio.file.{Files, Path}
import scaled._

/** Manages reloading an in memory structure when the files from which it was loaded have changed.
  * The structure is loaded and parsed on demand the first time it is requested, and subsequent
  * requests result in a comparison of the last load time against the latest last modified time of
  * the underlying files and a reparse, if necessary.
  */
class Reloadable[T] (files :Seq[Path], parser :Seq[Path] => T) extends PropertyV[T] {

  /** A helper constructor for when a resource is backed by only one file. */
  def this (file :Path, parser :Path => T) = this(Seq(file), (s :Seq[Path]) => parser(s.head))

  private var lastLoaded = 0L
  private var value :T = _

  override def get :T = {
    val lastModified = files.map(f => Files.getLastModifiedTime(f).toMillis).max
    if (lastModified > lastLoaded) {
      lastLoaded = lastModified
      value = parser(files)
    }
    value
  }

  override def apply () = get
}
