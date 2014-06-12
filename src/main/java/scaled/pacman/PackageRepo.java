//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class PackageRepo {

  /** Used to observe package goings on. */
  public static interface Observer {
    void packageAdded (Package pkg);
    void packageRemoved (Package pkg);
  }

  /** A hook for Scaled to observe package goings on. */
  public Observer observer;

  /** The top-level Scaled metadata directory. */
  public final Path metaDir = locateMetaDir();

  /** Returns all currently installed packages. */
  public Iterable<Package> packages () {
    return _pkgs.values();
  }

  /** Returns the package named {@code name}, if any. */
  public Optional<Package> packageByName (String name) {
    // TODO: map packages by name?
    for (Package pkg : packages()) if (pkg.name.equals(name)) return Optional.of(pkg);
    return Optional.empty();
  }

  /** Returns the package identified by {@code source}, if any. */
  public Optional<Package> packageBySource (Source source) {
    return Optional.ofNullable(_pkgs.get(source));
  }

  /** Emits a warning message. By default these go to stderr. */
  public void warn (String msg) {
    System.err.println(msg);
  }

  /** Emits a warning message. By default these go to stderr. */
  public void warn (String msg, Throwable t) {
    warn(msg);
    t.printStackTrace(System.err);
  }

  /** Resolves the dependencies for {@code pkg}. TODO: scope. */
  public DependTree resolveDepends (Package pkg) {
    DependTree tree = new DependTree(pkg.toDependNode());
    resolveDepends(pkg.depends, tree.root);
    return tree;
  }

  private void resolveDepends (List<Depend> depends, DependTree.Node into) {
    List<RepoId> mvnIds = new ArrayList<>();
    for (Depend dep : depends) {
      if (dep.id instanceof RepoId) mvnIds.add((RepoId)dep.id);
      else {
        Package pkg = _pkgs.get((Source)dep.id);
        if (pkg != null) {
          DependTree.Node pnode = pkg.toDependNode();
          resolveDepends(pkg.depends, pnode);
          into.children.add(pnode);
        } else warn("Missing package for depend [owner=" + into.id + ", source=" + dep.id + "]");
      }
    }
    // now resolve the maven dependencies all at once and add them at this level
    // into.children.addAll(_mvn.resolve(mvnIds));
  }

  /** Creates a class loader for {@code pkg}. */
  public PackageLoader createLoader (Package pkg) {
    List<RepoId> mvnIds = new ArrayList<>();
    List<PackageLoader> packageDeps = new ArrayList<>();
    for (Depend dep : pkg.depends) {
      if (dep.id instanceof RepoId) mvnIds.add((RepoId)dep.id);
      else {
        Package dpkg = _pkgs.get((Source)dep.id);
        if (dpkg != null) packageDeps.add(dpkg.loader());
        else warn("Missing package depend [owner=" + pkg.source + ", source=" + dep.id + "]");
      }
    }

    // use a linked hash set because Capsule sometimes returns duplicate dependencies, so this will
    // filter them out; but we need to preserve iteration order
    LinkedHashSet<Path> mavenDeps = new LinkedHashSet<>();
    // if the package has Maven dependencies, resolve those and add them to maven deps
    if (!mvnIds.isEmpty()) {
      // compute the transitive set of Maven depends already handled by our package dependencies;
      // we'll omit those from our Maven deps because we want to "inherit" them
      Set<Path> haveMavenDeps = new HashSet<>();
      for (PackageLoader dep : packageDeps) dep.accumMavenDeps(haveMavenDeps);
      for (Path path : _mvn.resolve(mvnIds)) if (!haveMavenDeps.contains(path)) mavenDeps.add(path);
    }
    return new PackageLoader(pkg.source, pkg.classesDir(), mavenDeps, packageDeps);
  }

  /** Resolves the dependencies for {@code pkg} into a properly ordered list of class loaders. */
  public List<ClassLoader> resolveLoaders (Package pkg) {
    DependTree tree = resolveDepends(pkg);
    List<ClassLoader> loaders = new ArrayList<>();
    for (Path path : tree.linearize()) {
      try {
        ClassLoader loader = _loaders.get(path);
        if (loader == null) { _loaders.put(
          path, loader = new URLClassLoader(new URL[] { path.toUri().toURL() }));
            warn("Created loader " + path);
        }
        loaders.add(loader);
      } catch (MalformedURLException e) {
        warn("Unable to compute URL for path: " + path, e);
      }
    }
    return loaders;
  }

  public PackageRepo () throws IOException {
    // create our top-level and Packages directories (if needed)
    Files.createDirectories(metaDir);
    _pkgsDir = metaDir.resolve("Packages");
    Files.createDirectories(_pkgsDir);

    // resolve all packages in our packages directory
    // (TODO: if this ends up being too slow, cache the results)
    Files.walkFileTree(_pkgsDir, new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult visitFile (Path dir, BasicFileAttributes attrs) {
        if (!Files.isDirectory(dir)) return FileVisitResult.CONTINUE;
        Path pkgFile = dir.resolve("package.scaled");
        if (!Files.exists(pkgFile)) return FileVisitResult.CONTINUE; // descend into subdirs
        try {
          Package pkg = new Package(PackageRepo.this, pkgFile);
          // log any errors noted when resolving this package info
          if (!pkg.errors.isEmpty()) {
            warn("ERRORS in " + pkg.root + "/package.scaled:");
            for (String error : pkg.errors) warn("- " + error);
          }
          _pkgs.put(pkg.source, pkg);
          if (observer != null) observer.packageAdded(pkg);
        } catch (Exception e) {
          warn("Unable to process package: "+ pkgFile, e);
        }
        return FileVisitResult.SKIP_SUBTREE; // stop descending
      }
    });
  }

  // TODO: platform specific app dirs
  private Path locateMetaDir () {
    Path homeDir = Paths.get(System.getProperty("user.home"));
    return homeDir.resolve(Paths.get("Library", "Application Support", "Scaled"));
  }

  private final Path _pkgsDir;
  private final Map<Source,Package> _pkgs = new HashMap<>();
  private final Map<Path,ClassLoader> _loaders = new HashMap<>();
  private final MavenResolver _mvn = new MavenResolver();
  // private final IvyResolver _ivy = new IvyResolver();
}
