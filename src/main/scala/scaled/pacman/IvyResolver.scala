//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

import java.nio.file.{Paths, Path, Files}
import java.net.URLClassLoader

/** Handles dependencies from the local Ivy repository. */
class IvyResolver {

  val (cacheDir, localDir) = {
    val ivyDir = Paths.get(System.getProperty("user.home"), ".ivy2")
    (ivyDir.resolve("cache"), ivyDir.resolve("local"))
  }

  /** Resolves the supplied Ivy dependency (and its transitive dependencies) and returns a
    * classloader which can deliver classes therefrom.
    */
  def resolveDepend (id :RepoId) :Option[PackageLoader] = {
    def loader (path :Path) :PackageLoader = new PackageLoader(id.toString, path) {
      override protected def resolveDependLoaders = Nil // TODO
    }
    val kindDir = s"${id.kind}s"
    val localFile = localDir.resolve(
      Paths.get(id.groupId, id.artifactId, id.version, kindDir, s"${id.artifactId}.${id.kind}"))
    val cacheFile = cacheDir.resolve(
      Paths.get(id.groupId, id.artifactId, kindDir, s"${id.artifactId}-${id.version}.${id.kind}"))
    if (Files.exists(localFile)) Some(loader(localFile))
    else if (Files.exists(cacheFile)) Some(loader(cacheFile))
    else None
  }
}
