//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Contains runtime metadata for one of a package's modules. */
public class Module implements Comparable<Module> {

  /** The string {@code module.scaled} for all to share and enjoy. */
  public static String FILE = "module.scaled";

  /** The identifier of the default module for a package. If a package declares no modules, it will
    * implicitly contain the default module, rooted at the package root and containing depends
    * obtained from the package. */
  public static String DEFAULT = "<default>";

  /** The package that contains this module. */
  public final Package pkg;

  /** This module's simple name. */
  public final String name;

  /** The root of this module, generally a directory. */
  public final Path root;

  /** A source that identifies this module. */
  public final Source source;

  /** This module' depends. */
  public final List<Depend> depends;

  /** This module's intra-package depends. */
  public final Set<String> localDepends = new HashSet<>();

  /** Creates a module info with the supplied metadata. */
  public Module (Package pkg, String name, Path root, Source source, Config cfg) {
    this.pkg  = pkg;
    this.name = name;
    this.root = root;
    this.source = source;
    this.depends = cfg.resolve("depend", new Config.DependListP(Depend.Scope.COMPILE));

    // compute our local depends
    for (Depend dep : depends) {
      if (!dep.isSource()) continue;
      Source depsrc = (Source)dep.id;
      if (depsrc.packageSource().equals(pkg.source)) localDepends.add(depsrc.module());
    }
  }

  /** Returns true if this is the default module in a single-module package, false otherwise. */
  public boolean isDefault () {
    return name == DEFAULT;
  }

  /** Returns a class loader for loading classes from this module and its depends. */
  public ModuleLoader loader () {
    if (_loader == null) _loader = pkg.repo.createLoader(this);
    return _loader;
  }

  public Path mainDir () { return root.resolve("src").resolve("main"); }
  public Map<String,Path> sourceDirs () throws IOException {
    Map<String,Path> dirs = new HashMap<>();
    Files.list(mainDir()).forEach(dir -> {
      dirs.put(dir.getFileName().toString(), dir);
    });
    return dirs;
  }
  public Path resourcesDir () { return mainDir().resolve("resources"); }

  public Path outputDir () { return root.resolve("target"); }
  public Path classesDir () { return outputDir().resolve("classes"); }

  // from interface Comparable
  public int compareTo (Module other) {
    if (localDepends.contains(other.name)) return 1;
    else if (other.localDepends.contains(name)) return -1;
    else return name.compareTo(other.name);
  }

  @Override public String toString () {
    return pkg.name + "#" + name;
  }

  private ModuleLoader _loader;
}
