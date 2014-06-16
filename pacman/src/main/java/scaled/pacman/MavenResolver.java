//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import capsule.DependencyManager;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class MavenResolver {

  public final Path m2repo = Paths.get(System.getProperty("user.home")).
    resolve(".m2").resolve("repository");
  public final DependencyManager capsule = new DependencyManager(m2repo, null, false, false);

  public List<Path> resolve (List<RepoId> ids) {
    List<String> coords = new ArrayList<>();
    for (RepoId id : ids) coords.add(id.toCoord());
    return capsule.resolveDependencies(coords, "jar");
  }
}
