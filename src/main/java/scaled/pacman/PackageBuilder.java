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

  public PackageBuilder (MavenResolver mvn, Package pkg) {
    _mvn = mvn;
    _pkg = pkg;
  }

  /** Cleans out this package's build results directory. */
  public void clean () throws IOException {
    Files.walkFileTree(_pkg.classesDir(), new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult visitFile (Path file, BasicFileAttributes attrs)
      throws IOException {
        if (!attrs.isDirectory()) Files.delete(file);
        return FileVisitResult.CONTINUE;
      }
      @Override public FileVisitResult postVisitDirectory (Path dir, IOException exn)
      throws IOException {
        if (exn != null) throw exn;
        Files.delete(dir);
        return FileVisitResult.CONTINUE;
      }
    });
  }

  /** Builds this package. */
  public void build () throws IOException {
    // create the build output directory
    Files.createDirectories(_pkg.classesDir());

    // TODO: copy resources into classes dir

    // now build whatever source we find in the project
    Map<String,Path> srcDirs = _pkg.sourceDirs();

    // if we have scala sources, use scalac to build scala+java code
    Path scalaDir = srcDirs.get("scala"), javaDir = srcDirs.get("java");
    if (scalaDir != null) buildScala(scalaDir, javaDir);
    else if (javaDir != null) buildJava(javaDir);
    else System.err.println("No scala or java source found in " + _pkg.sourceDir());
    // TODO: support other languages
  }

  private void buildScala (Path scalaDir, Path javaDir) throws IOException {
    String scalacId = "org.scala-lang:scala-compiler:2.11.0";
    List<Path> scalacCP = _mvn.resolve(Arrays.asList(RepoId.parse(scalacId)));
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

    if (exec(cmd) != 0) throw new IOException("Java build failed.");
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

  private int exec (List<String> cmd) throws IOException {
    System.out.println("Exec in " + _pkg.root);
    System.out.println("  " + cmd.get(0));
    boolean cont = false;
    for (int ii = 1, ll = cmd.size(); ii < ll; ii++) {
      String arg = cmd.get(ii);
      if (cont) { System.out.println(" " + arg); cont = false; }
      else if (!arg.startsWith("-")) System.out.println("     " + arg);
      else { System.out.print("     " + arg); cont = true; }
    }

    ProcessBuilder proc = new ProcessBuilder(cmd.toArray(new String[cmd.size()]));
    proc.directory(_pkg.root.toFile());
    proc.inheritIO();
    Process p = proc.start();
    try {
      return p.waitFor();
    } catch (InterruptedException e) {
      System.err.println("Interrupted waiting for exec.");
      return -1;
    }
  }

  private final MavenResolver _mvn;
  private final Package _pkg;
}
