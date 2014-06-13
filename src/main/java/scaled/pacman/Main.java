//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/** The main command line entry point for the Scaled Package Manager. */
public class Main {

  public static String USAGE =
    "Usage: spam <command>\n" +
    "\n" +
    "where <command> is one of:\n" +
    "\n" +
    "  list                          lists all installed packages\n" +
    "  search text                   lists all packages in directory which match text\n" +
    "  install [pkg-name | pkg-url]  installs package (by name or url) and its depends\n" +
    "  info [pkg-name | --all]       prints detailed info on pkg-name (or all packages)\n" +
    "  depends pkg-name              prints the depend tree for pkg-name\n" +
    "  classpath pkg-name            prints the classpath for pkg-name\n" +
    "  build pkg-name [--deps]       cleans and builds pkg-name (and its depends if --deps)\n" +
    "  clean pkg-name [--deps]       cleans pkg-name (and its depends if --deps)\n" +
    "  run pkg-name class [arg ...]  runs class from pkg-name with args";

  public static Printer out = new Printer(System.out);
  public static PackageRepo repo;
  public static PackageDirectory index = new PackageDirectory();

  public static void main (String[] args) throws IOException {
    if (args.length == 0) fail(USAGE);

    // create our package repository and grind our packages
    repo = new PackageRepo();

    // read our index (downloading the initial package if necessary)
    initIndex();

    // we'll introduce proper arg parsing later; for now KISS
    switch (args[0]) {
    case      "list": list(); break;
    case    "search": search(optarg(args, 1, "")); break;
    case   "install": install(args[1]); break;
    case      "info": info(args[1]); break;
    case   "depends": depends(args[1]); break;
    case "classpath": classpath(args[1]); break;
    case     "build": build(args[1], optarg(args, 2, "").equals("--deps")); break;
    case     "clean": clean(args[1], optarg(args, 2, "").equals("--deps")); break;
    case       "run": run(args[1], args[2], tail(args, 3)); break;
    default: fail(USAGE); break;
    }
  }

  private static final String IDX_GIT_URL = "https://github.com/scaled/scaled-directory.git";
  private static void initIndex () {
    try {
      Path idir = repo.pkgsDir.resolve("scaled-directory");
      if (!Files.exists(idir)) {
        System.out.println("* Fetching Scaled package directory...");
        VCSDriver.get(Source.VCS.GIT).checkout(new URI(IDX_GIT_URL), idir);
      }
      index.init(repo.log, idir);
    } catch (Exception e) {
      repo.log.log("Error inititalizing package directory, 'search' and 'install' by name " +
                   "are not going to work.", "error", e);
    }
  }

  private static void install (String whence) throws IOException {
    // see if this is a known package name
    PackageDirectory.Entry entry = index.byName.get(whence);
    if (entry != null) {
      PackageFetcher.install(repo, entry.source);
    }
    // if this looks like a URL, try checking it out by URL
    else if (whence.contains(":")) {
      try {
        PackageFetcher.install(repo, Source.parse(whence));
      } catch (URISyntaxException e) {
        // look for this package in our directory
        fail("Invalid package URL '" + whence + "': " + e.getMessage());
      }
    }
    else fail("Unknown package '" + whence + "'");
  }

  private static void search (String text) {
    String ltext = text.toLowerCase();
    for (PackageDirectory.Entry entry : index.entries) {
      if (entry.name.toLowerCase().contains(ltext) ||
          entry.descrip.toLowerCase().contains(ltext)) {
        System.out.println(entry.name + " " + entry.descrip);
      }
    }
  }

  private static void list () {
    out.println("Installed packages:");
    List<String[]> info = new ArrayList<>();
    for (Package pkg : repo.packages()) {
      info.add(new String[] { pkg.name, pkg.source.toString() });
    }
    out.printCols(info, "No packages found.");
  }

  private static void info (String pkgName) {
    if (!pkgName.equals("--all")) onPackage(pkgName, Main::printInfo);
    else for (Package pkg : repo.packages()) {
      out.println("------------------------------------------------------------");
      printInfo(pkg);
      out.println("");
    }
  }

  private static void printInfo (Package pkg) {
    out.println("Package: "+ pkg.name);
    out.printCols(Arrays.asList(new String[] { "Install:", pkg.root.toString() },
                                new String[] { "Source:",  pkg.source.toString() },
                                new String[] { "Version:", pkg.version },
                                new String[] { "Descrip:", pkg.descrip },
                                new String[] { "Web URL:", pkg.weburl },
                                new String[] { "License:", pkg.license }), "", 1);
  }

  private static void depends (String pkgName) {
    onPackage(pkgName, pkg -> pkg.loader().dump(System.out, "", new HashSet<>()));
  }

  private static void classpath (String pkgName) {
    onPackage(pkgName, pkg -> {
        for (Path path : pkg.loader().classpath()) out.println(path);
    });
  }

  private static void build (String pkgName, boolean deps) {
    onPackage(pkgName, pkg -> {
      for (Package bpkg : packageOrDeps(pkg, deps)) {
        try { new PackageBuilder(repo, bpkg).build(); }
        catch (Exception e) {
          System.err.println("Failure invoking 'build' in: " + bpkg.root);
          e.printStackTrace(System.err);
        }
      }
    });
  }

  private static void clean (String pkgName, boolean deps) {
    onPackage(pkgName, pkg -> {
      for (Package bpkg : packageOrDeps(pkg, deps)) {
        try { new PackageBuilder(repo, bpkg).clean(); }
        catch (Exception e) {
          System.err.println("Failure invoking 'clean' in: " + bpkg.root);
          e.printStackTrace(System.err);
        }
      }
    });
  }

  private static List<Package> packageOrDeps (Package pkg, boolean deps) {
    return deps ? repo.packageDepends(pkg) : Collections.singletonList(pkg);
  }

  private static void run (String pkgName, String classname, String[] args) {
    onPackage(pkgName, pkg -> {
      try {
        Class<?> clazz = pkg.loader().loadClass(classname);
        clazz.getMethod("main", String[].class).invoke(null, (Object)args);
      } catch (Exception e) {
        e.printStackTrace(System.err);
      }
    });
  }

  private static void onPackage (String name, Consumer<Package> fn) {
    Optional<Package> po = repo.packageByName(name);
    if (po.isPresent()) fn.accept(po.get());
    else fail("Unknown package: "+ name);
  }

  private static String optarg (String[] args, int idx, String defval) {
    return (args.length > idx) ? args[idx] : defval;
  }

  private static String[] tail (String[] args, int from) {
    String[] rest = new String[args.length-from];
    System.arraycopy(args, from, rest, 0, rest.length);
    return rest;
  }

  private static void fail (String msg) {
    System.err.println(msg);
    System.exit(255);
  }
}
