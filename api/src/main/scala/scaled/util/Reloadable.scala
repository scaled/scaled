//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.InputStream
import java.nio.file.{Files, Path}
import scaled._

/** Manages reloading an in memory structure when the file from which it was loaded has changed. The
  * structure is loaded and parsed on demand the first time it is requested, and subsequent requests
  * result in a comparison of the last load time against the last modified time of the underlying
  * file and a reparse, if necessary.
  */
class Reloadable[T] (file :Path, parser :Path => T) extends PropertyV[T] {

  private var lastLoaded = 0L
  private var value :T = _

  override def get :T = {
    val lastModified = Files.getLastModifiedTime(file).toMillis
    if (lastModified > lastLoaded) {
      lastLoaded = lastModified
      value = parser(file)
    }
    value
  }

  override def apply () = get
}
