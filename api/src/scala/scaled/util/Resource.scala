//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scaled._

/** Simplifies the conversion of one or more files that are bundled with a package into some
  * runtime data structure. It handles automatically reparsing said files if any of them change
  * (when possible), and provides some convenience methods for getting at the file contents.
  */
abstract class Resource {

  /** Returns the file names of each of this resource's files. */
  def names :Seq[String]

  /** Returns input streams for this resource's files. */
  def streams :Seq[InputStream]

  /** Returns the contents of this resource's files as lists of lines. */
  def lines :Seq[SeqV[String]] = streams map Resource.readLines

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
      lazy val names = sources.map(s => Paths.get(s.getPath).getFileName.toString)
      override def streams = sources.map(_.openStream)
      override protected def lastModified = 1L
    }
    else new Resource {
      val paths = sources.map(s => Paths.get(s.toURI))
      override def names = paths.map(_.getFileName.toString)
      override def streams = paths.map(Files.newInputStream(_))
      override protected def lastModified = paths.map(
        f => Files.getLastModifiedTime(f).toMillis).max
    }
  }

  /** Reads the lines from `in`, which must contain UTF8 encoded text. */
  def readLines (in :InputStream) :SeqV[String] = {
    val lines = Seq.builder[String]
    try {
      val reader = new BufferedReader(new InputStreamReader(in))
      def loop (line :String) :Unit = if (line != null) {
        lines += line
        loop(reader.readLine)
      }
      loop(reader.readLine)
    }
    finally in.close()
    lines.build()
  }
}
