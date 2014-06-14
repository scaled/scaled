//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Handles the compilation of a package's code.
 */
public class PackageBuilder {

  public PackageBuilder (PackageRepo repo, Package pkg) {
    _repo = repo;
    _pkg = pkg;
  }

  /** Cleans out the build results directory for all modules in this package. */
  public void clean () throws IOException {
    for (Module mod : _pkg.modules()) {
      Filez.deleteAll(mod.classesDir());
    }
  }

  /** Builds all modules in this package. */
  public void build () throws IOException {
    for (Module mod : _pkg.modules()) build(mod);
  }

  /** Builds the specified module. */
  public void build (Module mod) throws IOException {
    // create the build output directory
    Files.createDirectories(mod.classesDir());

    // if a resources directory exists, copy that over
    Path rsrcDir = mod.resourcesDir();
    if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, mod.classesDir());

    // now build whatever source we find in the project
    Map<String,Path> srcDirs = mod.sourceDirs();

    // if we have scala sources, use scalac to build scala+java code
    Path scalaDir = srcDirs.get("scala"), javaDir = srcDirs.get("java");
    // compile scala first in case there are java files that depend on scala's; scalac does some
    // fiddling to support mixed compilation but it doesn't generate bytecode for .javas
    if (scalaDir != null) buildScala(mod, scalaDir, javaDir);
    if (javaDir != null) buildJava(mod, javaDir);
    // TODO: support other languages
  }

  private void buildScala (Module mod, Path scalaDir, Path javaDir) throws IOException {
    List<String> cmd = new ArrayList<>();
    cmd.add(findJavaHome().resolve("bin").resolve("java").toString());

    String scalacId = "org.scala-lang:scala-compiler:2.11.0";
    cmd.add("-cp");
    cmd.add(classpathToString(_repo.mvn.resolve(Arrays.asList(RepoId.parse(scalacId)))));
    cmd.add("scala.tools.nsc.Main");

    cmd.add("-d"); cmd.add(mod.root.relativize(mod.classesDir()).toString());
    List<Path> cp = buildClasspath(mod);
    if (!cp.isEmpty()) { cmd.add("-classpath"); cmd.add(classpathToString(cp)); }
    if (javaDir != null) addSources(mod.root, javaDir, ".java", cmd);
    addSources(mod.root, scalaDir, ".scala", cmd);

    Exec.exec(mod.root, cmd).expect(0, "Scala build failed.");
  }

  private void buildJava (Module mod, Path javaDir) throws IOException {
    List<String> cmd = new ArrayList<>();
    cmd.add(findJavaHome().resolve("bin").resolve("javac").toString());

    cmd.add("-source"); cmd.add("1.8");
    cmd.add("-target"); cmd.add("1.8");
    cmd.add("-d"); cmd.add(mod.root.relativize(mod.classesDir()).toString());
    List<Path> cp = buildClasspath(mod);
    if (!cp.isEmpty()) { cmd.add("-cp"); cmd.add(classpathToString(cp)); }
    addSources(mod.root, javaDir, ".java", cmd);

    Exec.exec(mod.root, cmd).expect(0, "Java build failed.");
  }

  private void addSources (Path root, Path dir, String suff, List<String> into) throws IOException {
    Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult visitFile (Path file, BasicFileAttributes attrs)
      throws IOException {
        // TODO: allow symlinks to source files? that seems wacky...
        if (attrs.isRegularFile() && file.getFileName().toString().endsWith(suff)) {
          into.add(root.relativize(file).toString());
        }
        return FileVisitResult.CONTINUE;
      }
    });
  }

  private List<Path> buildClasspath (Module mod) {
    List<Path> cp = mod.loader().classpath();
    cp.remove(mod.classesDir());
    return cp;
  }

  private String classpathToString (List<Path> paths) {
    String pathSep = System.getProperty("path.separator");
    StringBuilder sb = new StringBuilder();
    for (Path path : paths) {
      if (sb.length() > 0) sb.append(pathSep);
      sb.append(path);
    }
    return sb.toString();
  }

  private Path findJavaHome () throws IOException {
    Path jreHome = Paths.get(System.getProperty("java.home"));
    Path javaHome = jreHome.getParent();
    if (isJavaHome(javaHome)) return javaHome;
    if (isJavaHome(jreHome)) return jreHome;
    throw new IllegalStateException("Unable to find java in " + jreHome + " or " + javaHome);
  }

  private boolean isJavaHome (Path javaHome) {
    return Files.exists(javaHome.resolve("bin").resolve("java"));
  }

  private final PackageRepo _repo;
  private final Package _pkg;
}
