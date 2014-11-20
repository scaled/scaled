//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import com.google.common.base.Charsets
import com.google.common.io.CharStreams
import java.io.InputStream
import java.io.InputStreamReader
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import scaled._

/** Simplifies the conversion of one or more files that are bundled with a package into some
  * runtime data structure. It handles automatically reparsing said files if any of them change
  * (when possible), and provides some convenience methods for getting at the file contents.
  */
abstract class Resource {

  /** Returns input streams for this resource's files. */
  def streams :Seq[InputStream]

  /** Returns the contents of this resource's files as lists of lines. */
  def lines :Seq[SeqV[String]] = streams map { in =>
    try CharStreams.readLines(new InputStreamReader(in, Charsets.UTF_8)).toSeqV
    finally in.close()
  }

  /** Returns a property which converts this resource to a `T` lazily. If the resource supports
    * change detection, the property will be regenerated the next time it is requested after the
    * resource has been modified. */
  def toProperty[T] (parser :Resource => T) :PropertyV[T] = new PropertyV[T] {
    private var lastLoaded = 0L
    private var value :T = _

    override def apply () = get
    override def get :T = {
      val lastMod = lastModified
      if (lastModified > lastLoaded) {
        lastLoaded = lastModified
        value = parser(Resource.this)
      }
      value
    }
  }

  protected def lastModified :Long
}

object Resource {

  /** Creates a resource from a single URL. */
  def apply[T] (source :URL) :Resource = apply(Seq(source))

  /** Creates a resource from a list of URLs. */
  def apply (sources :Seq[URL]) :Resource = {
    // if the URLs are not all file: URLs, we can't do auto-reloading
    if (sources.exists(_.getProtocol != "file")) new Resource() {
      override def streams = sources.map(_.openStream)
      override protected def lastModified = 0L
    }
    else new Resource {
      val paths = sources.map(s => Paths.get(s.toURI))
      override def streams = paths.map(Files.newInputStream(_))
      override protected def lastModified = paths.map(
        f => Files.getLastModifiedTime(f).toMillis).max
    }
  }
}
