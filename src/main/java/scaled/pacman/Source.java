//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.net.URI;
import java.net.URISyntaxException;

public class Source implements Depend.Id {

  public static enum VCS {
    GIT, HG, SVN;

    public static VCS parse (String vcs) {
      try { return Enum.valueOf(VCS.class, vcs.toUpperCase()); }
      catch (Exception e) { throw new IllegalArgumentException("Unknown VCS: " + vcs); }
    }

    @Override public String toString () {
      return super.toString().toLowerCase();
    }
  }

  public static Source parse (String text) throws URISyntaxException {
    String[] bits = text.split(":", 2);
    if (bits.length != 2) throw new IllegalArgumentException("Invalid VCS URI: "+ text);
    return parse(bits[0], bits[1]);
  }

  public static Source parse (String vcs, String url) throws URISyntaxException {
    return new Source(VCS.parse(vcs), new URI(url));
  }

  public final VCS vcs;
  public final URI url;

  public Source (VCS vcs, URI url) {
    this.vcs = vcs;
    this.url = url;
  }

  @Override public String conflictId () { return toString(); }
  @Override public String toString () { return vcs + ":" + url; }
  @Override public int hashCode () { return vcs.hashCode() ^ url.hashCode(); }
  @Override public boolean equals (Object other) {
    return (other instanceof Source) && vcs == ((Source)other).vcs &&
      url.equals(((Source)other).url);
  }
}
