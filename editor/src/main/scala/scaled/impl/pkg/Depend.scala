//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

/** Models a Maven or Ivy dependency. */
case class Depend (groupId :String, artifactId :String, version :String, kind :String)

/** [[Depend]] related utilities. */
object Depend {

  /** Parses the supplied URL into a `Depend`. The URL must be of the form:
    * `groupId:artifactId:version:kind`. `kind` can be omitted and `jar` will be assumed.
    */
  def fromURL (url :String) = url.split(":") match {
    case Array(groupId, artifactId, version, kind) =>
      Some(Depend(groupId, artifactId, version, kind))
    case Array(groupId, artifactId, version) =>
      Some(Depend(groupId, artifactId, version, "jar"))
    case other =>
      None
  }
}
