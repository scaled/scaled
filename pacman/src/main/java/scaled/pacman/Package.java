//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/** Contains runtime metadata for an installed package. */
public class Package {

  /** The string {@code package.scaled} for all to share and enjoy. */
  public static String FILE = "package.scaled";

  /** The repository from whence this package came. */
  public final PackageRepo repo;

  /** The root of this package, generally a directory. */
  public final Path root;

  public final Source source;
  public final String name;
  public final String version;
  public final String license;
  public final String weburl;
  public final String descrip;

  public final List<String> errors;

  /** Returns all modules contained in this packge. These are returned topologically sorted, such
    * that any module which depends on another module in this package will show up later in the
    * list than the module on which it depends. */
  public Iterable<Module> modules () {
    return new TreeSet<>(_modules.values());
  }

  /** Returns the module with name {@code name} or null. */
  public Module module (String name) {
    return _modules.get(name);
  }

  /** Creates a package info from the supplied `package.scaled` file.
    * The file is assumed to be in the top-level directory of the package in question. */
  public Package (PackageRepo repo, Path file) throws IOException {
    this(repo, file.getParent(), Files.readAllLines(file));
  }

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  public Package (PackageRepo repo, Path root, Iterable<String> lines) {
    this(repo, root, new Config(lines));
  }

  public Package (PackageRepo repo, Path root, Config cfg) {
    this.repo = repo;
    this.root = root;
    source  = cfg.resolve("source",  Config.SourceP);
    name    = cfg.resolve("name",    Config.StringP);
    version = cfg.resolve("version", Config.StringP);
    license = cfg.resolve("license", Config.StringP);
    weburl  = cfg.resolve("weburl",  Config.StringP); // todo UrlP
    descrip = cfg.resolve("descrip", Config.StringP);

    // if no modules were defined, create the default module using package config
    List<String> mods = cfg.resolve("module", Config.StringListP);
    if (mods.isEmpty()) {
      _modules.put(Module.DEFAULT, new Module(this, Module.DEFAULT, root, source, cfg));
    }

    // we're done with the package config, so accumulate any errors
    errors = cfg.finish();

    // this will noop if no modules were defined, but we structure the code this way because we need
    // errors to be initialized before we parse our module configs
    for (String mname : mods) {
      Path mroot = root.resolve(mname);
      try {
        Config mcfg = new Config(Files.readAllLines(mroot.resolve("module.scaled")));
        _modules.put(mname, new Module(this, mname, mroot, source.moduleSource(mname), mcfg));
        errors.addAll(mcfg.finish());
      } catch (IOException ioe) {
        errors.add("Failed to parse module " + mname + ": " + ioe);
      }
    }
  }

  /** Returns sources for all packages on which any module in this package depends. */
  public Set<Source> packageDepends () {
    Set<Source> deps = new HashSet<>();
    for (Module mod : modules()) {
      for (Depend dep : mod.depends) if (dep.isSource()) {
        deps.add(((Source)dep.id).packageSource());
      }
    }
    return deps;
  }

  @Override public String toString () {
    return (" source=" + source  + "\n" +
            "   name=" + name    + "\n" +
            "version=" + version + "\n" +
            "license=" + license + "\n" +
            " weburl=" + weburl  + "\n" +
            "descrip=" + descrip + "\n" +
            "modules=" + _modules.keySet() + "\n" +
            " errors=" + errors);
  }

  private final Map<String,Module> _modules = new HashMap<>();
}
