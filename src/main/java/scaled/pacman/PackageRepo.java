//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
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

  /** A log instance whence logging is sent. This goes to stderr, unless/until Scaled takes over the
    * JVM at which point it reroutes it to the *messages* buffer. */
  public Log log = new Log() {
    public void log (String msg) { System.err.println(msg); }
    public void log (String msg, Throwable error) { log(msg); error.printStackTrace(System.err); }
  };

  /** The top-level Scaled metadata directory. */
  public final Path metaDir = locateMetaDir();

  /** The directory into which packages are installed. */
  public final Path pkgsDir = getMetaDir("Packages");

  /** Used to resolve Maven artifacts. */
  public final MavenResolver mvn = new MavenResolver();

  /** Creates (if necessary) and returns a directory in the top-level Scaled metadata directory. */
  public Path getMetaDir (String name) throws IOException {
    Path dir = metaDir.resolve(name);
    Files.createDirectories(dir);
    return dir;
  }

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

  /** Returns a list of {@code pkg}'s transitive package dependencies. The list will be ordered such
    * that each package will appear later in the list than all packages on which it depends. Note:
    * {@code pkg} is included at the end of the list. */
  public List<Package> packageDepends (Package pkg) {
    LinkedHashMap<Source,Package> pkgs = new LinkedHashMap<>();
    addPackageDepends(pkgs, pkg);
    return new ArrayList<>(pkgs.values());
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
        else log.log("Missing package depend", "owner", pkg.source, "source", dep.id);
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
      for (Path path : mvn.resolve(mvnIds)) if (!haveMavenDeps.contains(path)) mavenDeps.add(path);
    }
    return new PackageLoader(pkg.source, pkg.classesDir(), mavenDeps, packageDeps);
  }

  public PackageRepo () throws IOException {
    // resolve all packages in our packages directory (TODO: use cache if this is too slow)
    Files.walkFileTree(pkgsDir, FOLLOW_LINKS, MAX_PKG_DEPTH, new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult preVisitDirectory (Path dir, BasicFileAttributes attrs) {
        Path pkgFile = dir.resolve(Package.FILE);
        if (!Files.exists(pkgFile)) return FileVisitResult.CONTINUE; // descend into subdirs
        addPackage(pkgFile);
        return FileVisitResult.SKIP_SUBTREE; // stop descending
      }
    });
  }

  public boolean addPackage (Path pkgFile) {
    try {
      Package pkg = new Package(PackageRepo.this, pkgFile);
      // log any errors noted when resolving this package info
      if (!pkg.errors.isEmpty()) {
        log.log("ERRORS in " + pkg.root + "/package.scaled:");
        for (String error : pkg.errors) log.log("- " + error);
      }
      _pkgs.put(pkg.source, pkg);
      if (observer != null) observer.packageAdded(pkg);
      return true;
    } catch (Exception e) {
      log.log("Unable to process package: "+ pkgFile, e);
      return false;
    }
  }

  private void addPackageDepends (LinkedHashMap<Source,Package> pkgs, Package pkg) {
    // stop if we've already added this package's depends
    if (pkgs.containsKey(pkg.source)) return;
    // add all of this package's depends
    for (Depend dep : pkg.depends) if (dep.isSource()) {
      Package dpkg = _pkgs.get(dep.id);
      if (dpkg == null) log.log("Missing depend!", "pkg", pkg.source, "dep", dep.id);
      else addPackageDepends(pkgs, dpkg);
    }
    // then add this package
    pkgs.put(pkg.source, pkg);
  }

  private Path locateMetaDir () {
    // if our metadir has been overridden, use the specified value
    String root = System.getProperty("scaled.meta");
    if (root != null) return Paths.get(root);

    // TODO: platform specific app dirs
    Path homeDir = Paths.get(System.getProperty("user.home"));
    return homeDir.resolve(Paths.get("Library", "Application Support", "Scaled"));
  }

  private final Map<Source,Package> _pkgs = new HashMap<>();

  private static final Set<FileVisitOption> FOLLOW_LINKS = Collections.singleton(
    FileVisitOption.FOLLOW_LINKS);
  private static final int MAX_PKG_DEPTH = 6;
}
