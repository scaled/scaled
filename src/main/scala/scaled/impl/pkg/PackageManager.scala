//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import com.google.common.collect.HashMultimap
import java.io.File
import java.net.URLClassLoader
import java.util.regex.Pattern
import reactual.Future
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled.impl._

class PackageManager (app :Main) {
  import scala.collection.convert.WrapAsScala._

  /** Resolves the class for the mode named `name`. */
  def mode (major :Boolean, name :String) :Future[Class[_]] =
    lookup(modeMap(major), name, "major mode")

  /** Resolves the class for the service with classname `name`. */
  def service (name :String) :Future[Class[_]] = lookup(serviceMap, name, "service")

  /** Returns the name of all modes provided by all packages. */
  def modes (major :Boolean) :Iterable[String] = modeMap(major).keySet

  /** Detects the major mode that should be used to edit `buf`. */
  def detectMode (buf :BufferImpl) :String = {
    // checks for -*- mode: somemode -*- on the first or second line
    def fileLocal :Option[String] = None // TODO
    // if the file starts with #!, detects based on "interpreter"
    def interp :Option[String] = buf.line(0).asString match {
      case text if (text startsWith "#!") =>
        // break #!/usr/bin/perl -w into tokens, filtering out known meaningless tokens
        val tokens = text.substring(2).split("[ /]").filterNot(skipToks)
        tokens.map(i => (i, interps.get(i))) collectFirst {
          case (interp, ms) if (!ms.isEmpty) =>
            if (ms.size > 1) warn("Multiple modes registered to handle interpreter '$interp': $ms")
            ms.head
        }
      case _ => None
    }
    // matches the file name against all registered mode regular expressions
    def pattern (name :String) :Option[String] = {
      val ms = patterns collect { case (p, m) if (p.matcher(name).matches()) => m }
      if (ms.size > 1) warn(s"Multiple modes match buffer name '$name': $ms")
      ms.headOption
    }
    fileLocal orElse interp orElse pattern(buf.file.getName) getOrElse "text"
  }
  private val skipToks = Set("", "usr", "local", "bin", "env", "opt")

  /** Returns the set of minor modes that should be auto-activated for `tags`. */
  def minorModes (tags :Array[String]) :Set[String] = Set() ++ tags flatMap (minorTags.get _)

  /** A mapping from repo URL to package. Repo URL is the unique global identifier for a package, and
    * is what is used to express inter-package dependencies. */
  private[pkg] val pkgs = MMap[String,Package]()

  private type Finder = String => Class[_]
  private val serviceMap = MMap[String,Finder]()
  private val majorMap = MMap[String,Finder]()
  private val minorMap = MMap[String,Finder]()
  private def modeMap (major :Boolean) = if (major) majorMap else minorMap

  private val patterns  = ArrayBuffer[(Pattern,String)]()
  private val interps   = HashMultimap.create[String,String]()
  private val minorTags = HashMultimap.create[String,String]()

  private def lookup (map :MMap[String,Finder], name :String, thing :String) :Future[Class[_]] =
    map.get(name).map(_.apply(name)) match {
      case Some(mode) => Future.success(mode)
      case None => Future.failure(new Exception(s"Unknown $thing: $name"))
    }

  // resolve our "built-in" package, which we locate via the classloader
  getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs foreach { url =>
    if ((url.getProtocol == "file") && !(url.getPath endsWith ".jar")) {
      // resolve this package immediately, on the main thread because we need our basic modes to be
      // available immediately; TODO: really? maybe we could defer those as well?
      addPackage(new Package(this, PackageInfo.builtin(new File(url.getPath))))
    }
  }

  // resolve all packages in our packages directory
  private val pkgsDir = Filer.requireDir(new File(app.metaDir, "Packages"))
  Filer.descendDirs(pkgsDir) { dir =>
    val pkgFile = new File(dir, "package.scaled")
    if (!pkgFile.exists) true // descend into subdirs
    else {
      app.exec.execute(new Runnable() {
        override def run () = resolvePackage(PackageInfo(pkgFile))
      })
      false // stop descending
    }
  }

  private def addPackage (pkg :Package) {
    // TODO: report errors in pkg.info
    pkgs.put(pkg.info.repo, pkg)
    // map this package's major and minor modes, and services
    pkg.majors.keySet foreach { majorMap.put(_, pkg.major _) }
    pkg.minors.keySet foreach { minorMap.put(_, pkg.minor _) }
    pkg.services foreach { serviceMap.put(_, pkg.service _) }
    // map the file patterns and interpreters defined by this package's major modes
    pkg.patterns.asMap foreach { case (m, ps) => ps foreach { p =>
      try patterns += (Pattern.compile(p) -> m)
      catch {
        case e :Exception => warn(s"Mode $m specified invalid pattern: $p: $e")
      }
    }}
    pkg.interps.asMap foreach { case (m, is) =>
      is foreach { i => interps.put(i, m) }
    }
    // map the tags defined by this pattern's minor modes
    minorTags.putAll(pkg.minorTags)
    // println(s"Added package $pkg")
  }

  private def warn (msg :String) = println(msg) // TODO

  // NOTE: this is called on a background thread
  private def resolvePackage (info :PackageInfo) {
    // println(s"Resolving package $info")
    // TODO: make sure this package is compiled
    // create the package and scan its classes
    val pkg = new Package(this, info)
    // lastly map this package's information back on our main thread
    onMainThread { addPackage(pkg) }
  }
}
