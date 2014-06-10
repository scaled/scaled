//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable.{Map => MMap}

object PackageManager {

  /** Used to observe package goings on. */
  trait Observer {
    def packageAdded (pkg :Package) :Unit
    def packageRemoved (pkg :Package) :Unit
  }

  /** A hook for Scaled to observe package goings on. */
  var observer :Observer = _

  /** The top-level Scaled metadata directory. */
  val metaDir :Path = { val dir = locateMetaDir ; Files.createDirectories(dir) ; dir }

  /** Returns all currently installed packages. */
  def packages :Iterable[Package] = pkgs.values

  /** Emits a warning message. By default these go to stderr. */
  def warn (msg :String) :Unit = System.err.println(msg)

  /** Emits a warning message. By default these go to stderr. */
  def warn (msg :String, t :Throwable) :Unit = { warn(msg) ; t.printStackTrace(System.err) }

  /** Resolves the specified package dependency, returning a classloader that can be used to load
    * classes from that dependency. */
  def resolveDepend (forpkg :Package)(depend :Depend) :Option[PackageLoader] = depend match {
    case MavenDepend(repoId)  => mvn.resolveDepend(repoId)
    case IvyDepend(repoId)    => ivy.resolveDepend(repoId)
    case SourceDepend(source) => pkgs.get(source).map(_.loader) orElse {
      warn(s"Missing project dependency [pkg=${forpkg.name}, src=$source]"); None
    }
  }

  private val pkgsDir = {
    val dir = metaDir.resolve("Packages") ; Files.createDirectories(dir) ; dir
  }
  private val pkgs = MMap[Source,Package]()
  private val mvn = new MavenResolver()
  private val ivy = new IvyResolver()

  // resolve all packages in our packages directory (TODO: if this ends up being too slow, then
  // cache the results of our scans and load that instead)
  Files.walkFileTree(pkgsDir, new SimpleFileVisitor[Path]() {
    override def visitFile (dir :Path, attrs :BasicFileAttributes) = {
      if (!Files.isDirectory(dir)) FileVisitResult.CONTINUE
      else {
        val pkgFile = dir.resolve("package.scaled")
        if (!Files.exists(pkgFile)) FileVisitResult.CONTINUE // descend into subdirs
        else {
          try {
            val pkg = Package(pkgFile)
            // log any errors noted when resolving this package info
            pkg.errors foreach warn
            pkgs.put(pkg.source, pkg)
            if (observer != null) observer.packageAdded(pkg)
          }
          catch {
            case e :Exception => warn(s"Unable to process package: $pkgFile", e)
          }
          FileVisitResult.SKIP_SUBTREE // stop descending
        }
      }
    }
  })

  // TODO: platform specific app dirs
  private def locateMetaDir :Path = {
    val homeDir = Paths.get(System.getProperty("user.home"))
    homeDir.resolve(Paths.get("Library", "Application Support", "Scaled"))
  }

  // TODO: install package phase where we download and install a package, install its dependencies
  // and ensure that everything is compiled and ready to run
}
