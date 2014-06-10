//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

// TODO: other scopes?
sealed trait Scope
case object Compile extends Scope
case object Test extends Scope

// TODO: model kind?
case class RepoId (
  groupId :String, artifactId :String, version :String, kind :String, scope :Scope
) extends Package.Id

sealed trait Depend
case class SourceDepend (source :Source) extends Depend
case class MavenDepend (id :RepoId) extends Depend
case class IvyDepend (id :RepoId) extends Depend

/** [[Depend]] related utilities. */
object Depend {

  /** Parses a string representation of a [[Depend]]. */
  def parse (url :String) :Depend = url.split(":", 2) match {
    case Array("mvn", rest) => MavenDepend(parseRepoId(rest))
    case Array("ivy", rest) => IvyDepend(parseRepoId(rest))
    case Array(vcs,   url ) => SourceDepend(Source.parse(vcs, url))
  }

  // parses a repo depend: repo:groupId:artifactId:version:kind:scope
  private def parseRepoId (text :String) :RepoId = text.split(":") match {
    case Array(groupId, artifactId, version, kind, scope) =>
      RepoId(groupId, artifactId, version, kind, parseScope(scope))
    case Array(groupId, artifactId, version, kind) =>
      RepoId(groupId, artifactId, version, kind, Compile)
    case Array(groupId, artifactId, version) =>
      RepoId(groupId, artifactId, version, "jar", Compile)
    case other => throw new IllegalArgumentException(
      s"Invalid repo id: $text (expect 'groupId:artifactId:version:scope')")
  }

  private def parseScope (scope :String) = scope match {
    case "compile" => Compile
    case "test" => Test
    case _ => throw new IllegalArgumentException(s"Invalid scope: $scope")
  }
}
