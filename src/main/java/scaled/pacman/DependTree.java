//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Contains a resolved tree of dependencies. Supports printing those dependencies as well as
 * linearizing them (resolving conflicts in the process).
 */
public class DependTree {

  public static abstract class Node {
    public final Depend.Id id;
    public final List<Node> children = new ArrayList<>();
    public abstract Path path ();

    public void dump (String indent) {
      System.out.println(indent + id);
      for (Node child : children) child.dump(indent + ".");
    }

    @Override public String toString () { return id.toString(); }

    protected Node (Depend.Id id) {
      this.id = id;
    }

    private void addTo (LinkedHashMap<String,Path> lmap) {
      String cid = id.conflictId();
      if (!lmap.containsKey(cid)) lmap.put(cid, path());
      for (Node child : children) child.addTo(lmap);
    }
  }

  public final Node root;

  public DependTree (Node root) {
    this.root = root;
  }

  public void dump () {
    root.dump("");
  }

  public Iterable<Path> linearize () {
    LinkedHashMap<String,Path> lmap = new LinkedHashMap<>();
    root.addTo(lmap);
    return lmap.values();
  }
}
