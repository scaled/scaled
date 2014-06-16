//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.nio.file.{Files, Path}
import scaled.Logger

/** Utilities for dealing with `.properties` files, which Scaled mainly uses for configuration. */
object Properties {
  import scala.collection.convert.WrapAsScala._

  /** Reads the contents of properties file `file` and calls `accum` with each key/value pair, as
    * they are encountered. Any invalid entries are reported to `log`.
    */
  def read (log :Logger, file :Path)(accum :(String, String) => Unit) {
    read(log, file.getFileName.toString, Files.readAllLines(file))(accum)
  }

  /** Parses the properties data in `lines` and calls `accum` with each key/value pair, as they are
    * encountered. Any invalid entries are reported to `log`.
    */
  def read (log :Logger, name :String, lines :Iterable[String])(accum :(String, String) => Unit) {
    def isComment (l :String) = (l startsWith "#") || (l.length == 0)
    // TODO: make this handle all the proper .properties files bits (escapes, multilines, etc.)
    lines map(_.trim) filterNot(isComment) foreach { _ split(":", 2) match {
      case Array(key, value) => accum(key.trim, value.trim)
      case other => log.log(s"$name contains invalid line:\n${other.mkString(":")}")
    }}
  }
}
