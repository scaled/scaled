//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File
import java.net.URLClassLoader

/** Handles dependencies from the local Ivy repository. */
class IvyResolver {

  val (cacheDir, localDir) = {
    val homeDir = new File(System.getProperty("user.home"))
    val ivyDir = new File(homeDir, ".ivy2")
    (new File(ivyDir, "cache"), new File(ivyDir, "local"))
  }

  /** Resolves the supplied Ivy dependency (and its transitive dependencies) and returns a
    * classloader which can deliver classes therefrom.
    */
  def resolveDepend (depend :Depend) :Option[ClassLoader] = {
    val kindDir = s"${depend.kind}s"
    val localFile = file(localDir, depend.groupId, depend.artifactId, depend.version, kindDir,
                         s"${depend.artifactId}.${depend.kind}")
    val cacheFile = file(cacheDir, depend.groupId, depend.artifactId, kindDir,
                         s"${depend.artifactId}-${depend.version}.${depend.kind}")
    if (localFile.exists) Some(loader(localFile))
    else if (cacheFile.exists) Some(loader(cacheFile))
    else None
  }

  private def loader (file :File) :URLClassLoader = new URLClassLoader(Array(file.toURI.toURL))

  private def file (root :File, subs :String*) = (root /: subs)(new File(_, _))
}
