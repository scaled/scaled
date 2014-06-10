//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.file.{Files, Path}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

/** Package related bits. */
object Package {
  import scala.collection.convert.WrapAsScala._

  /** Used to identify packages. */
  trait Id {
  }

  /** Used to report dependency trees. */
  case class Node (id :Id, depends :List[Node]) {
    def dump (indent :String = "") {
      println(indent + id)
      depends.foreach(_.dump(indent + "."))
    }
  }

  /** Creates a package info from the supplied `package.scaled` file. The file is assumed to be in
    * the top-level directory of the package in question. */
  def apply (file :Path) :Package = apply(file.getParent, Files.readAllLines(file))

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :InputStream) :Package =
    apply(root, Seq() ++ new BufferedReader(new InputStreamReader(lines)).lines.iterator)

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  def apply (root :Path, lines :Iterable[String]) :Package = new Package(root, new Config(lines))
}

/** Contains runtime metadata for an installed package. */
class Package (val root :Path, config :Config) {
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

  /** The loader for classes in this package. */
  val loader :PackageLoader = new PackageLoader(source, classesDir) {
    override protected def resolveDependLoaders =
      depends.flatMap(PackageManager.resolveDepend(Package.this))
  }

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
