//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import com.google.common.collect.HashMultimap
import java.io.File
import java.net.URLClassLoader
import java.util.regex.Pattern
import reactual.{Future, Signal}
import scala.collection.mutable.{ArrayBuffer, Map => MMap, Set => MSet}
import scaled.impl._
import scaled.util.Error

class PackageManager (app :Main) {
  import scala.collection.convert.WrapAsScala._

  /** A signal emitted when a package is installed. */
  val packageAdded = Signal[Package]()

  /** A signal emitted when a package is uninstalled. */
  val packageRemoved = Signal[Package]()

  /** Returns all currently installed packages. */
  def packages :Iterable[Package] = pkgs.values

  /** Resolves the class for the mode named `name`. */
  def mode (major :Boolean, name :String) :Future[Class[_]] =
    lookup(modeMap(major), name, "major mode")

  /** Resolves the implementation class for the service with fq classname `name`. */
  def service (name :String) :Option[Class[_]] = serviceMap.get(name).map(_.apply(name))

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
    println(s"Detecting mode for ${buf.name}")
    fileLocal orElse interp orElse pattern(buf.name) getOrElse "text"
  }
  private val skipToks = Set("", "usr", "local", "bin", "env", "opt")

  /** Returns the set of minor modes that should be auto-activated for `tags`. */
  def minorModes (tags :Array[String]) :Set[String] = Set() ++ tags flatMap (minorTags.get _)

  /** Resolves the specified package dependency, returning a classloader that can be used to load
    * classes from that dependency. Dependencies URLs are of the form:
    *  # git:https://github.com/scaled/foo-service.git
    *  # git:https://code.google.com/p/scaled-bar-service/
    *  # hg:https://code.google.com/p/scaled-baz-service/
    *  # svn:https://scaled-pants-service.googlecode.com/svn/trunk
    *  # mvn:com.google.guava:guava:16.0.1:jar
    *  # ivy:com.google.guava:guava:16.0.1:jar
    *
    * Dependencies in the form of a DVCS URL will have been checked out into `pkgsDir` and built.
    * This happens during package installation, _not_ during this dependency resolution process.
    * Dependencies prefixed by `mvn:` will be resolved from the local Maven repository, and those
    * prefixed by `ivy:` will be resolved from the local Ivy repository. These dependencies will
    * also be assumed to already exist, having been downloaded during package installation.
    */
  def resolveDepend (info :PackageInfo)(depURL :String) :Option[ClassLoader] = {
    def fail (msg :String) = { warn(s"$msg [pkg=${info.name}, dep=$depURL]"); None }
    depURL.split(":", 2) match {
      case Array("mvn", depURL) => Depend.fromURL(depURL) match {
        case Some(depend) => mvn.resolveDepend(depend)
        case None         => fail(s"Invalid Maven dependency URL")
      }
      case Array("ivy", depURL) => Depend.fromURL(depURL) match {
        case Some(depend) => ivy.resolveDepend(depend)
        case None         => fail(s"Invalid Ivy dependency URL")
      }
      case Array(vcs, url) => pkgs.get(depURL).map(_.loader) orElse
        fail(s"Missing project dependency")
      case other           => fail(s"Invalid project dependency")
    }
  }

  /** A mapping from `srcurl` to package. `srcurl` is the unique global identifier for a package, and
    * is what is used to express inter-package dependencies. */
  private val pkgs = MMap[String,Package]()

  private val mvn = new MavenResolver(app)
  private val ivy = new IvyResolver()

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
      case None       => Error.futureFeedback(s"Unknown $thing: $name")
    }

  // resolve our "built-in" package(s), which we locate via the classloader
  getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs foreach { url =>
    if ((url.getProtocol == "file") && !(url.getPath endsWith ".jar"))
      addBuiltin(new File(url.getPath))
  }

  // resolve all packages in our packages directory (TODO: if this ends up being too slow, then
  // cache the results of our scans and load that instead)
  private val pkgsDir = Filer.requireDir(new File(app.metaDir, "Packages"))

  // if we have more than one built-in package then we're running in dev mode, which means we do
  // package management a bit differently: we don't use custom classloaders (this enables JRebel to
  // work) and we don't load packages from the Packages directory (because those would require
  // custom classloaders to function); TODO: maybe separate these two modes
  if (pkgs.size == 1) {
    val mainDep = Some(pkgs.head._2.info.srcurl)
    Filer.descendDirs(pkgsDir) { dir =>
      val pkgFile = new File(dir, "package.scaled")
      if (!pkgFile.exists) true // descend into subdirs
      else {
        addPackage(new Package(this, PackageInfo(pkgFile, mainDep)))
        false // stop descending
      }
    }
  } else {
    app.log("*** Package manager running in development mode.")
    app.log("*** Only packages visible via the development classpath will be loaded.")
  }

  // scans up from `dir` looking for 'package.scaled' file; then adds package from there
  private def addBuiltin (dir :File) {
    if (dir != null) {
      val pfile = new File(dir, "package.scaled")
      if (!pfile.exists) addBuiltin(dir.getParentFile)
      else addPackage(new Package(this, PackageInfo(pfile, None)))
    }
  }

  private def addPackage (pkg :Package) {
    // TODO: report errors in pkg.info
    pkgs.put(pkg.info.srcurl, pkg)

    // map this package's major and minor modes, services and plugins
    pkg.majors.keySet foreach { majorMap.put(_, pkg.major _) }
    pkg.minors.keySet foreach { minorMap.put(_, pkg.minor _) }
    pkg.services.keySet foreach { serviceMap.put(_, pkg.service _) }
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
    // tell any interested parties about this new package
    packageAdded.emit(pkg)
    // println(s"Added package $pkg")
  }

  // TODO: install package phase where we download and install a package, install its dependencies
  // and ensure that everything is compiled and ready to run

  private def warn (msg :String) = println(msg) // TODO
}
