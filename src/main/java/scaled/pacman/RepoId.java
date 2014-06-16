//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

public class RepoId implements Depend.Id {

  // parses a repo depend: repo:groupId:artifactId:version:kind
  public static RepoId parse (String text) {
    String[] bits = text.split(":", 5);
    if (bits.length < 3) throw new IllegalArgumentException(
      "Invalid repo id: "+ text +" (expect 'groupId:artifactId:version')");
    String kind = bits.length > 3 ? bits[3] : "jar";
    return new RepoId(bits[0], bits[1], bits[2], kind);
  }

  public final String groupId;
  public final String artifactId;
  public final String version;
  public final String kind;

  public RepoId (String groupId, String artifactId, String version, String kind) {
    this.groupId = groupId;
    this.artifactId = artifactId;
    this.version = version;
    this.kind = kind;
  }

  public String toCoord () {
    return groupId + ":" + artifactId + ":" + version;
  }

  @Override public String toString () {
    return groupId + ":" + artifactId + ":" + version + ":" + kind;
  }

  @Override public int hashCode () {
    return groupId.hashCode() ^ artifactId.hashCode() ^ version.hashCode() ^ kind.hashCode();
  }

  @Override public boolean equals (Object other) {
    if (!(other instanceof RepoId)) return false;
    RepoId oid = (RepoId)other;
    return (groupId.equals(oid.groupId) && artifactId.equals(oid.artifactId) &&
            version.equals(oid.version) && kind.equals(oid.kind));
  }
}
