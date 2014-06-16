//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

public class SystemId implements Depend.Id {

  // parses a system depend: platform:artifact:version
  public static SystemId parse (String text) {
    String[] bits = text.split(":", 3);
    if (bits.length < 3) throw new IllegalArgumentException(
      "Invalid system id: "+ text +" (expect 'platform:artifact:version')");
    return new SystemId(bits[0], bits[1], bits[2]);
  }

  public final String platform;
  public final String artifact;
  public final String version;

  public SystemId (String platform, String artifact, String version) {
    this.platform = platform;
    this.artifact = artifact;
    this.version = version;
  }

  @Override public String toString () {
    return platform + ":" + artifact + ":" + version;
  }

  @Override public int hashCode () {
    return platform.hashCode() ^ artifact.hashCode() ^ version.hashCode();
  }

  @Override public boolean equals (Object other) {
    if (!(other instanceof SystemId)) return false;
    SystemId oid = (SystemId)other;
    return (platform.equals(oid.platform) && artifact.equals(oid.artifact) &&
            version.equals(oid.version));
  }

}
