//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File

import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.io.Source

case class PackageInfo (
  root    :File,
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

  def isBuiltIn = srcurl == PackageInfo.builtInSrcURL
  def classesDir = new File(root, bindir)

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

  /** The `srcurl` for our built-in package. */
  final val builtInSrcURL = "git:https://github.com/samskivert/scaled.git"

  /** Creates a package info from the supplied `package.scaled` file. The file is assumed to be in
    * the top-level directory of the package in question. */
  def apply (file :File) :PackageInfo =
    fromFile(file.getParentFile, Source.fromFile(file))

  /** Creates a package info for the "built-in" package. This exports the modes and services defined
    * in the main Scaled source tree. */
  def builtin (classesDir :File) :PackageInfo =
    fromFile(classesDir.getParentFile, Source.fromString(s"""
   name: scaled
version: 1.0
descrip: Built-in services.
 weburl: https://github.com/samskivert/scaled/
 srcurl: $builtInSrcURL
license: New BSD
 srcdir: .
 bindir: ${classesDir.getName}
"""))

  private def fromFile (root :File, source :Source) = {
    val props = MMap[String,String]()
    var depends = List[String]()
    val errors = ArrayBuffer[String]()
    source.getLines.map(trim).filter(_.length > 0) foreach { line => line.split(":", 2) match {
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
                require("srcurl"), require("license"), builtInSrcURL :: depends,
                require("srcdir"), props.getOrElse("bindir", "classes"), errors)
  }

  private def trim (line :String) = line.indexOf('#') match {
    case -1 => line.trim
    case ii => line.substring(0, ii).trim
  }
}
