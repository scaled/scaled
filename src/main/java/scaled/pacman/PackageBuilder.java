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

  /** Cleans out this package's build results directory. */
  public void clean () throws IOException {
    Filez.deleteAll(_pkg.classesDir());
  }

  /** Builds this package. */
  public void build () throws IOException {
    // create the build output directory
    Files.createDirectories(_pkg.classesDir());

    // if a resources directory exists, copy that over
    Path rsrcDir = _pkg.resourcesDir();
    if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, _pkg.classesDir());

    // now build whatever source we find in the project
    Map<String,Path> srcDirs = _pkg.sourceDirs();

    // if we have scala sources, use scalac to build scala+java code
    Path scalaDir = srcDirs.get("scala"), javaDir = srcDirs.get("java");
    // compile scala first in case there are java files that depend on scala's; scalac does some
    // fiddling to support mixed compilation but it doesn't generate bytecode for .javas
    if (scalaDir != null) buildScala(scalaDir, javaDir);
    if (javaDir != null) buildJava(javaDir);
    // TODO: support other languages
  }

  private void buildScala (Path scalaDir, Path javaDir) throws IOException {
    List<String> cmd = new ArrayList<>();
    cmd.add(findJavaHome().resolve("bin").resolve("java").toString());

    String scalacId = "org.scala-lang:scala-compiler:2.11.0";
    cmd.add("-cp");
    cmd.add(classpathToString(_repo.mvn.resolve(Arrays.asList(RepoId.parse(scalacId)))));
    cmd.add("scala.tools.nsc.Main");

    cmd.add("-d"); cmd.add(_pkg.root.relativize(_pkg.classesDir()).toString());
    List<Path> cp = buildClasspath();
    if (!cp.isEmpty()) { cmd.add("-classpath"); cmd.add(classpathToString(cp)); }
    if (javaDir != null) addSources(javaDir, ".java", cmd);
    addSources(scalaDir, ".scala", cmd);

    Exec.exec(_pkg.root, cmd).expect(0, "Scala build failed.");
  }

  private void buildJava (Path javaDir) throws IOException {
    List<String> cmd = new ArrayList<>();
    cmd.add(findJavaHome().resolve("bin").resolve("javac").toString());

    cmd.add("-source"); cmd.add("1.8");
    cmd.add("-target"); cmd.add("1.8");
    cmd.add("-d"); cmd.add(_pkg.root.relativize(_pkg.classesDir()).toString());
    List<Path> cp = buildClasspath();
    if (!cp.isEmpty()) { cmd.add("-cp"); cmd.add(classpathToString(cp)); }
    addSources(javaDir, ".java", cmd);

    Exec.exec(_pkg.root, cmd).expect(0, "Java build failed.");
  }

  private void addSources (Path dir, String suff, List<String> into) throws IOException {
    Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult visitFile (Path file, BasicFileAttributes attrs)
      throws IOException {
        // TODO: allow symlinks to source files? that seems wacky...
        if (attrs.isRegularFile() && file.getFileName().toString().endsWith(suff)) {
          into.add(_pkg.root.relativize(file).toString());
        }
        return FileVisitResult.CONTINUE;
      }
    });
  }

  private List<Path> buildClasspath () {
    List<Path> cp = _pkg.loader().classpath();
    cp.remove(_pkg.classesDir());
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
