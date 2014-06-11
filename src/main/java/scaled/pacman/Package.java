//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/** Contains runtime metadata for an installed package. */
public class Package {

  /** The repository from whence this package came. */
  public final PackageRepo repo;

  /** The root of this package, generally a directory. */
  public final Path root;

  public final Source source;
  public final String name;
  public final String version;
  public final String descrip;
  public final String weburl;
  public final String license;
  public final String srcdir;
  public final String bindir;
  public final List<Depend> depends;

  public final List<String> errors;

  /** Creates a package info from the supplied `package.scaled` file.
    * The file is assumed to be in the top-level directory of the package in question. */
  public Package (PackageRepo repo, Path file) throws IOException {
    this(repo, file.getParent(), Files.readAllLines(file));
  }

  /** Creates a package info from the `package.scaled` contents in `lines`. */
  public Package (PackageRepo repo, Path root, InputStream lines) throws IOException {
    this(repo, root, new BufferedReader(new InputStreamReader(lines)).lines().
         collect(Collectors.toList()));
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
    descrip = cfg.resolve("descrip", Config.StringP);
    weburl  = cfg.resolve("weburl",  Config.StringP); // todo UrlP
    license = cfg.resolve("license", Config.StringP);
    srcdir  = cfg.resolve("srcdir",  Config.StringP); // todo PackageDirP?
    bindir  = cfg.resolve("bindir",  Config.StringP);
    depends = cfg.resolve("depend",  new Config.DependP(Depend.Scope.COMPILE));
    errors  = cfg.finish();
  }

  /** Returns a resolved dependency tree node for this project. */
  public DependTree.Node toDependNode () {
    return new DependTree.Node(source) {
      @Override public Path path () { return classesDir(); }
    };
  }

  /** Returns a class loader for loading classes from this package and its depends. */
  public PackageLoader loader () {
    if (_loader == null) _loader = repo.createLoader(this);
    return _loader;
  }

  public Path classesDir () {
    return root.resolve(bindir);
  }

  @Override public String toString () {
    return (" source=" + source  + "\n" +
            "   name=" + name    + "\n" +
            "version=" + version + "\n" +
            "descrip=" + descrip + "\n" +
            " weburl=" + weburl  + "\n" +
            "license=" + license + "\n" +
            "depends=" + depends + "\n" +
            " srcdir=" + srcdir  + "\n" +
            " bindir=" + bindir  + "\n" +
            " errors=" + errors);
  }

  private PackageLoader _loader;
}
