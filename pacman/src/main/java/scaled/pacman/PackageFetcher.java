//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;

/**
 * Handles checking out and updating packages via external DVCS programs.
 */
public class PackageFetcher {

  /** Installs the package referenced by {@code source} and all of its depends. */
  public static void install (PackageRepo repo, Source source) throws IOException {
    PackageFetcher pf = installer(repo, source);
    repo.log.log("Cloning " + source + " into temp dir...");
    pf.checkout();
    Package pkg = pf.installDepends();
    repo.log.log("Building " + pkg.name + "...");
    PackageBuilder pb = new PackageBuilder(repo, pkg);
    pb.clean();
    pb.build();
    repo.log.log("Installing " + source + " into Packages/" + pkg.name + "...");
    pf.install(pkg);
  }

  /** Creates a package fetcher for installing {@code source}. The fetcher will be configured with a
    * temporary directory. This allows one to check out and build a new package before moving it
    * into the Scaled packages directory.
    */
  public static PackageFetcher installer (PackageRepo repo, Source source) throws IOException {
    Path temp = Files.createTempDirectory(repo.metaDir("Scratch"), "install");
    // delete this directory on JVM shutdown, if we haven't done it already
    Runtime.getRuntime().addShutdownHook(new Thread() { public void run () {
      try { Filez.deleteAll(temp); }
      catch (IOException e) { e.printStackTrace(System.err); }
    }});
    return new PackageFetcher(repo, source, temp);
  }

  public PackageFetcher (PackageRepo repo, Source source, Path pkgDir) {
    _repo = repo;
    _source = source;
    _pkgDir = pkgDir;
    _vcs = VCSDriver.get(source.vcs);
  }

  /** Checks out the this package into {@code pkgDir}. */
  public void checkout () throws IOException {
    if (!_vcs.exists(_source.url, _pkgDir)) _vcs.checkout(_source.url, _pkgDir);
    else { _vcs.fetch(_pkgDir); _vcs.update(_pkgDir); }
  }

  /** Ensures that all depends of this package have been installed.
    * This assumes that {@code pkgDir} points to a valid checkout of this project. */
  public Package installDepends () throws IOException {
    Package pkg = new Package(_repo, _pkgDir.resolve(Package.FILE));
    for (Source source : pkg.packageDepends()) {
      // this package is already installed
      if (_repo.packageBySource(source).isPresent()) continue;
      // otherwise we need to download and install this depend
      install(_repo, source);
    }
    return pkg;
  }

  public void install (Package pkg) throws IOException {
    Path target = _repo.packageDir(pkg.name);
    Files.move(_pkgDir, target, StandardCopyOption.ATOMIC_MOVE);
    _repo.addPackage(target.resolve(Package.FILE));
  }

  private final PackageRepo _repo;
  private final Source _source;
  private final Path _pkgDir;
  private final VCSDriver _vcs;
}
