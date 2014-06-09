//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.file.{Files, Path}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.io.Source

case class PackageInfo (
  root    :Path,
  name    :String,
  version :String,
  descrip :String,
  weburl  :String,
  srcurl  :String,
  license :String,
  depends :List[String],
  srcdir  :String,
  bindir  :String,
  errors  :Seq[String]) {

  def classesDir = root.resolve(bindir)

  override def toString =
    s"""|   name: $name
        |version: $version
        |descrip: $descrip
        | weburl: $weburl
        | srcurl: $srcurl
        |license: $license
        |depends: $depends
        | srcdir: $srcdir
        | bindir: $bindir
        | errors: $errors""".stripMargin
}

object PackageInfo {
  import scala.collection.convert.WrapAsScala._

  /** Creates a package info from the supplied `package.scaled` file. The file is assumed to be in
    * the top-level directory of the package in question. */
  def apply (file :Path) :PackageInfo = apply(file.getParent, Files.readAllLines(file))

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :InputStream) :PackageInfo =
    apply(root, Seq() ++ new BufferedReader(new InputStreamReader(lines)).lines.iterator)

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :Seq[String]) :PackageInfo = {
    val props = MMap[String,String]()
    var depends = List[String]()
    val errors = ArrayBuffer[String]()
    lines.map(trim).filter(_.length > 0) foreach { line => line.split(":", 2) match {
      case Array(key, value) =>
        val (tkey, tvalue) = (key.trim, value.trim)
        if (tkey == "depend") depends = tvalue :: depends
        else props += (tkey -> tvalue) // TODO: validate that prop has meaning?
      case _ => errors += s"Invalid: $line"
    }}
    def require (prop :String) = props.getOrElse(prop, {
      errors += s"Missing property '$prop'"
      "unknown"
    })
    PackageInfo(root, require("name"), require("version"), require("descrip"), require("weburl"),
                require("srcurl"), require("license"), depends, require("srcdir"),
                props.getOrElse("bindir", "classes"), errors)
  }

  private def trim (line :String) = line.indexOf('#') match {
    case -1 => line.trim
    case ii => line.substring(0, ii).trim
  }
}
