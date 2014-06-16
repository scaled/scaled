//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class SystemResolver {

  public final Path jreHome = Paths.get(System.getProperty("java.home"));
  public final Path javaHome = jreHome.getParent();
  public final Log log;

  public SystemResolver (Log log) {
    this.log = log;
  }

  public List<Path> resolve (Source owner, List<SystemId> ids) {
    List<Path> results = new ArrayList<>();
    try {
      for (SystemId id : ids) results.add(resolve(id));
    } catch (Exception e) {
      log.log("Failed to resolve system depend", "source", owner, "error", e);
    }
    return results;
  }

  private Path resolve (SystemId id) {
    if (id.platform.equals("jdk")) {
      if (id.artifact.equals("tools")) {
        return javaHome.resolve("lib").resolve("tools.jar");
      }
      throw new IllegalArgumentException("Unknown JDK artifact " + id);
    }
    throw new IllegalArgumentException("Unknown platform " + id);
  }
}
