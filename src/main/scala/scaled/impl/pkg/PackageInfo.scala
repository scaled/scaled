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
  builtIn :Boolean,
  errors  :Seq[String]) {

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

  /** Creates a package info from the supplied `package.scaled` file. The file is assumed to be in
    * the top-level directory of the package in question.
    *
    * @param mainDep the srcurl of the built-in package (if `Some`), on which we'll automatically
    * add a dependency. If `None`, this package is assumed to be a built-in package.
    */
  def apply (file :File, mainDep :Option[String]) :PackageInfo =
    parse(file.getParentFile, mainDep, Source.fromFile(file))

  private def parse (root :File, mainDep :Option[String], source :Source) = {
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
                require("srcurl"), require("license"), mainDep.toList ++  depends,
                // if we weren't supplied with a main dependency, then we're a built-in
                require("srcdir"), props.getOrElse("bindir", "classes"), !mainDep.isDefined, errors)
  }

  private def trim (line :String) = line.indexOf('#') match {
    case -1 => line.trim
    case ii => line.substring(0, ii).trim
  }
}
