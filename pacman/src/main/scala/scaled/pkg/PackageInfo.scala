//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pkg

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
  builtIn :Boolean,
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
    * the top-level directory of the package in question.
    *
    * @param mainDep the srcurl of the built-in package (if `Some`), on which we'll automatically
    * add a dependency. If `None`, this package is assumed to be a built-in package.
    */
  def apply (file :Path, mainDep :Option[String]) :PackageInfo =
    apply(file.getParent, Files.readAllLines(file), mainDep)

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :InputStream, mainDep :Option[String]) :PackageInfo =
    apply(root, Seq() ++ new BufferedReader(new InputStreamReader(lines)).lines.iterator, mainDep)

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :Seq[String], mainDep :Option[String]) :PackageInfo = {
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
                require("srcurl"), require("license"), mainDep.toList ++  depends,
                // if we weren't supplied with a main dependency, then we're a built-in
                require("srcdir"), props.getOrElse("bindir", "classes"), !mainDep.isDefined, errors)
  }

  private def trim (line :String) = line.indexOf('#') match {
    case -1 => line.trim
    case ii => line.substring(0, ii).trim
  }
}
