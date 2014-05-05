//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File
import java.net.URLClassLoader
import pomutil.{Dependency, DependResolver, POM}
import scaled.impl.Logger

/** Handles dependencies from the local Maven repository. */
class MavenResolver (log :Logger) {

  /** Resolves the supplied Maven dependency (and its transitive dependencies) and returns a
    * classloader which can deliver classes therefrom.
    */
  def resolveDepend (depend :Depend) :Option[ClassLoader] = {
    val pdep = Dependency(depend.groupId, depend.artifactId, depend.version, depend.kind)
    for {
      pfile <- pdep.localPOM
      pom <- POM.fromFile(pfile)
      res = new DependResolver(pom)
    } yield loader(depend, res.resolve(true))
  }

  private def loader (orig :Depend, deps :Seq[Dependency]) :URLClassLoader = {
    val (jars, errs) = partitionBy(deps)(dep => dep.localArtifact match {
      case Some(jar) => Left(jar)
      case None => Right(s"Missing local artifact for $dep")
    })
    errs foreach log.log
    println(s"$orig => $jars")
    new URLClassLoader(jars.map(_.toURI.toURL).toArray)
  }

  private def partitionBy[A,B,C] (seq :Seq[A])(fn :A => Either[B,C]) :(Seq[B], Seq[C]) = {
    val bb = Seq.newBuilder[B] ; val cc = Seq.newBuilder[C]
    seq foreach { a => fn(a) match {
      case Left(b)  => bb += b
      case Right(c) => cc += c
    }}
    (bb.result, cc.result)
  }

  private def file (root :File, subs :String*) = (root /: subs)(new File(_, _))
}
