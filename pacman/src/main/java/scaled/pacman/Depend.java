//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman;

import java.net.URISyntaxException;

public class Depend {

  public static interface Id {
  }

  // TODO: other scopes?
  public static enum Scope {
    COMPILE, TEST;
  }

  /** Parses a string representation of a [[Depend]]. */
  public static Depend parse (String url, Scope scope) throws URISyntaxException {
    String[] bits = url.split(":", 2);
    if (bits.length == 1) throw new IllegalArgumentException("Invalid depend URI: " + url);
    return new Depend(parseId(bits[0], bits[1]), scope);
  }

  private static Id parseId (String tag, String data) throws URISyntaxException {
    switch (tag) {
      case "mvn": return RepoId.parse(data);
      case "sys": return SystemId.parse(data);
      default:    return Source.parse(tag, data);
    }
  }

  public final Id id;
  public final Scope scope;

  public Depend (Id id, Scope scope) {
    this.id = id;
    this.scope = scope;
  }

  /** Returns true if this is a source depend. */
  public boolean isSource () {
    return (id instanceof Source);
  }

  @Override public String toString () { return id + ":" + scope.toString().toLowerCase(); }
  @Override public int hashCode () { return id.hashCode() ^ scope.hashCode(); }
  @Override public boolean equals (Object oo) {
    return (oo instanceof Depend) && ((Depend)oo).id.equals(id) && ((Depend)oo).scope == scope;
  }
}
