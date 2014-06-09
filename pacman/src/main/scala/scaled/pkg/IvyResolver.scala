//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pkg

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
  def resolveDepend (depend :Depend) :Option[ClassLoader] = {
    val kindDir = s"${depend.kind}s"
    val localFile = localDir.resolve(
      Paths.get(depend.groupId, depend.artifactId, depend.version, kindDir,
                s"${depend.artifactId}.${depend.kind}"))
    val cacheFile = cacheDir.resolve(
      Paths.get(depend.groupId, depend.artifactId, kindDir,
                s"${depend.artifactId}-${depend.version}.${depend.kind}"))
    if (Files.exists(localFile)) Some(loader(localFile))
    else if (Files.exists(cacheFile)) Some(loader(cacheFile))
    else None
  }

  private def loader (file :Path) :URLClassLoader = new URLClassLoader(Array(file.toUri.toURL)) {
    override def toString = s"IvyLoader($file)"
  }
}
