//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.file.{Files, Path}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object PackageInfo {
  import scala.collection.convert.WrapAsScala._

  /** Creates a package info from the supplied `package.scaled` file. The file is assumed to be in
    * the top-level directory of the package in question. */
  def apply (file :Path) :PackageInfo = apply(file.getParent, Files.readAllLines(file))

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :InputStream) :PackageInfo =
    apply(root, Seq() ++ new BufferedReader(new InputStreamReader(lines)).lines.iterator)

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :Iterable[String]) :PackageInfo =
    new PackageInfo(root, new Config(lines))
}

class PackageInfo (val root :Path, config :Config) {
  import Config._

  val source  :Source       = config("source",  SourceP)
  val name    :String       = config("name",    StringP)
  val version :String       = config("version", StringP)
  val descrip :String       = config("descrip", StringP)
  val weburl  :String       = config("weburl",  StringP) // todo UrlP
  val license :String       = config("license", StringP)
  val srcdir  :String       = config("srcdir",  StringP) // todo PackageDirP?
  val bindir  :String       = config("bindir",  StringP)
  val depends :List[Depend] = config("depend",  DependP)

  val errors = config.finish()

  def classesDir = root.resolve(bindir)

  override def toString =
    s"""| source: $source
        |   name: $name
        |version: $version
        |descrip: $descrip
        | weburl: $weburl
        |license: $license
        |depends: $depends
        | srcdir: $srcdir
        | bindir: $bindir
        | errors: $errors""".stripMargin
}
