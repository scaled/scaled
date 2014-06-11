//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman;

import java.net.MalformedURLException;

public class Depend {

  public static interface Id {
    String conflictId ();
  }

  // TODO: other scopes?
  public static enum Scope {
    COMPILE, TEST;
  }

  /** Parses a string representation of a [[Depend]]. */
  public static Depend parse (String url, Scope scope) throws MalformedURLException {
    String[] bits = url.split(":", 2);
    if (bits.length == 1) throw new IllegalArgumentException("Invalid depend URL: " + url);
    if (bits[0].equals("mvn")) return new Depend(RepoId.parse(bits[1]), scope);
    return new Depend(Source.parse(bits[0], bits[1]), scope);
  }

  public final Id id;
  public final Scope scope;

  public Depend (Id id, Scope scope) {
    this.id = id;
    this.scope = scope;
  }
}
