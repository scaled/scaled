//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable.{Map => MMap}

class PackageManager {

  /** The top-level Scaled metadata directory. */
  val metaDir :Path = { val dir = locateMetaDir ; Files.createDirectories(dir) ; dir }

  /** Returns all currently installed packages. */
  def packages :Iterable[Package] = pkgs.values

  /** Emits a warning message. By default these go to stderr. */
  def warn (msg :String) :Unit = System.err.println(msg)

  /** Emits a warning message. By default these go to stderr. */
  def warn (msg :String, t :Throwable) :Unit = { warn(msg) ; t.printStackTrace(System.err) }

  /** Resolves the specified package dependency, returning a classloader that can be used to load
    * classes from that dependency. Dependencies URLs are of the form:
    *  - git:https://github.com/scaled/foo-service.git
    *  - git:https://code.google.com/p/scaled-bar-service/
    *  - hg:https://code.google.com/p/scaled-baz-service/
    *  - svn:https://scaled-pants-service.googlecode.com/svn/trunk
    *  - mvn:com.google.guava:guava:16.0.1:jar
    *  - ivy:com.google.guava:guava:16.0.1:jar
    *
    * Dependencies in the form of a DVCS URL will have been checked out into `pkgsDir` and built.
    * This happens during package installation, _not_ during this dependency resolution process.
    * Dependencies prefixed by `mvn:` will be resolved from the local Maven repository, and those
    * prefixed by `ivy:` will be resolved from the local Ivy repository. These dependencies will
    * also be assumed to already exist, having been downloaded during package installation.
    */
  def resolveDepend (info :PackageInfo)(depend :Depend) :Option[ClassLoader] = {
    def fail (msg :String) = { warn(s"$msg [pkg=${info.name}, dep=$depend]"); None }
    depend match {
      case MavenDepend(repoId)  => mvn.resolveDepend(repoId)
      case IvyDepend(repoId)    => ivy.resolveDepend(repoId)
      case SourceDepend(source) => pkgs.get(source).map(_.loader) orElse fail(
        s"Missing project dependency")
    }
  }

  /** A mapping from `srcurl` to package. `srcurl` is the unique global identifier for a package,
    * and is what is used to express inter-package dependencies. */
  private val pkgs = MMap[Source,Package]()

  private val mvn = new MavenResolver(this)
  private val ivy = new IvyResolver()

  private val pkgsDir = {
    val dir = metaDir.resolve("Packages") ; Files.createDirectories(dir) ; dir
  }

  // resolve all packages in our packages directory (TODO: if this ends up being too slow, then
  // cache the results of our scans and load that instead)
  Files.walkFileTree(pkgsDir, new SimpleFileVisitor[Path]() {
    override def visitFile (dir :Path, attrs :BasicFileAttributes) = {
      if (!Files.isDirectory(dir)) FileVisitResult.CONTINUE
      else {
        val pkgFile = dir.resolve("package.scaled")
        if (!Files.exists(pkgFile)) FileVisitResult.CONTINUE // descend into subdirs
        else {
          addPackage(PackageInfo(pkgFile))
          FileVisitResult.SKIP_SUBTREE // stop descending
        }
      }
    }
  })

  protected def createPackage (info :PackageInfo) :Package = new Package(this, info)

  protected def addPackage (info :PackageInfo) :Package = {
    // log any errors noted when resolving this package info
    info.errors foreach warn

    // create our package and map it by srcurl
    val pkg = createPackage(info)
    pkgs.put(info.source, pkg)
    pkg
  }

  // TODO: platform specific app dirs
  private def locateMetaDir :Path = {
    val homeDir = Paths.get(System.getProperty("user.home"))
    homeDir.resolve(Paths.get("Library", "Application Support", "Scaled"))
  }

  // TODO: install package phase where we download and install a package, install its dependencies
  // and ensure that everything is compiled and ready to run
}
