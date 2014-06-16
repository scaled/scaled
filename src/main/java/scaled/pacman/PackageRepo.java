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
import java.util.Collection;
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

  /** Used to resolve Maven artifacts. */
  public final MavenResolver mvn = new MavenResolver();

  /** Used to resolve System artifacts. */
  public final SystemResolver sys = new SystemResolver(log);

  /** Creates (if necessary) and returns a directory in the top-level Scaled metadata directory. */
  public Path metaDir (String name) throws IOException {
    Path dir = metaDir.resolve(name);
    Files.createDirectories(dir);
    return dir;
  }

  /** Returns the directory in which all packages are installed. */
  public Path packagesDir () throws IOException {
    return metaDir("Packages");
  }

  /** Returns the directory in which a package named {@code name} should be installed. */
  public Path packageDir (String name) throws IOException {
    return packagesDir().resolve(name);
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

  /** Returns the module identified by {@code source}, if any. */
  public Optional<Module> moduleBySource (Source source) {
    Package pkg = _pkgs.get(source.packageSource());
    return Optional.ofNullable(pkg == null ? null : pkg.module(source.module()));
  }

  /** Returns a list of {@code pkg}'s transitive module dependencies. The list will be ordered such
    * that each package will appear later in the list than all packages on which it depends. Note:
    * {@code pkg} is included at the end of the list. */
  public List<Package> packageDepends (Package pkg) {
    LinkedHashMap<Source,Package> pkgs = new LinkedHashMap<>();
    addPackageDepends(pkgs, pkg);
    return new ArrayList<>(pkgs.values());
  }

  /** Creates a class loader for {@code pkg}. */
  public ModuleLoader createLoader (Module module) {
    List<RepoId> mvnIds = new ArrayList<>();
    List<SystemId> sysIds = new ArrayList<>();
    List<ModuleLoader> moduleDeps = new ArrayList<>();
    for (Depend dep : module.depends) {
      if (dep.id instanceof RepoId) mvnIds.add((RepoId)dep.id);
      else if (dep.id instanceof SystemId) sysIds.add((SystemId)dep.id);
      else {
        Optional<Module> dmod = moduleBySource((Source)dep.id);
        if (dmod.isPresent()) moduleDeps.add(dmod.get().loader());
        else log.log("Missing source depend", "owner", module.source, "source", dep.id);
      }
    }

    // use a linked hash set because Capsule sometimes returns duplicate dependencies, so this will
    // filter them out; but we need to preserve iteration order
    LinkedHashSet<Path> binDeps = new LinkedHashSet<>();
    // if the module has binary dependencies, resolve those and add them to maven deps
    if (!mvnIds.isEmpty() || !sysIds.isEmpty()) {
      // compute the transitive set of binary depends already handled by our module dependencies;
      // we'll omit those from our binary deps because we want to "inherit" them
      Set<Path> haveBinaryDeps = new HashSet<>();
      for (ModuleLoader dep : moduleDeps) dep.accumBinaryDeps(haveBinaryDeps);
      addFiltered(haveBinaryDeps, mvn.resolve(mvnIds), binDeps);
      addFiltered(haveBinaryDeps, sys.resolve(module.source, sysIds), binDeps);
    }
    return new ModuleLoader(module.source, module.classesDir(), binDeps, moduleDeps);
  }

  private void addFiltered (Set<Path> except, List<Path> source, Collection<Path> dest) {
    for (Path path : source) if (!except.contains(path)) dest.add(path);
  }

  public void init () throws IOException {
    // resolve all packages in our packages directory (TODO: use cache if this is too slow)
    Files.walkFileTree(packagesDir(), FOLLOW_LINKS, MAX_PKG_DEPTH, new SimpleFileVisitor<Path>() {
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
    // add all packages on which any of this package's modules depend
    for (Module mod : pkg.modules()) {
      for (Depend dep : mod.depends) if (dep.isSource()) {
        Source psrc = ((Source)dep.id).packageSource();
        Package dpkg = _pkgs.get(psrc);
        if (dpkg == null) log.log("Missing depend!", "mod", mod.source, "dep", dep.id);
        else addPackageDepends(pkgs, dpkg);
      }
    }
    // then add this package
    pkgs.put(pkg.source, pkg);
  }

  private Path locateMetaDir () {
    // if our metadir has been overridden, use the specified value
    String root = System.getProperty("scaled.meta");
    if (root != null) return Paths.get(root);

    Path homeDir = Paths.get(System.getProperty("user.home"));
    // if we're on a Mac, put things in ~/Library/Application Support/Scaled
    Path appSup = homeDir.resolve("Library").resolve("Application Support");
    if (Files.exists(appSup)) return appSup.resolve("Scaled");
    // otherwise use ~/.scaled (TODO: we can probably do better on Windows)
    else return homeDir.resolve(".scaled");
  }

  private final Map<Source,Package> _pkgs = new HashMap<>();

  private static final Set<FileVisitOption> FOLLOW_LINKS = Collections.singleton(
    FileVisitOption.FOLLOW_LINKS);
  private static final int MAX_PKG_DEPTH = 6;
}
