//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File
import java.net.{URL, URLClassLoader}
import pomutil.{Dependency, DependResolver, POM}
import scaled.impl.Logger

/** Handles dependencies from the local Maven repository. */
class MavenResolver (log :Logger) {

  /** Resolves the supplied Maven dependency (and its transitive dependencies) and returns a
    * classloader which can deliver classes therefrom.
    */
  def resolveDepend (depend :Depend) :Option[ClassLoader] = {
    val pdep = Dependency(depend.groupId, depend.artifactId, depend.version, depend.kind)
    pdep.localPOM match {
      case None => log.log(s"Unable to resolve POM for $pdep") ; None
      case Some(pfile) => POM.fromFile(pfile) match {
        case None => log.log(s"Unable to load POM from $pfile") ; None
        case Some(pom) =>
          val res = new DependResolver(pom) {
            // ignore non-compile depends; dependencies loaded here are always at least one step
            // away from the "root" depend, and hence should follow the standard Maven policy of
            // excluding all non-compile depends
            override def rootDepends (forTest :Boolean) =
              if (forTest) super.rootDepends(forTest)
              else pom.depends filter(_.scope == "compile")
          }
          Some(loader(depend, pdep +: res.resolve(false)))
      }
    }
  }

  private def loader (orig :Depend, deps :Seq[Dependency]) :URLClassLoader = {
    val urls = Array.newBuilder[URL]
    deps foreach { dep => dep.localArtifact match {
      case Some(jar) => urls += jar.toURI.toURL
      case None      => log.log(s"Missing local artifact for $dep")
    }}
    new URLClassLoader(urls.result)
  }

  private def file (root :File, subs :String*) = (root /: subs)(new File(_, _))
}
