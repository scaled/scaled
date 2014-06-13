//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

/** File utilities. */
public class Filez {

  /** Deletes {@code dir} and all of its contents. */
  public static void deleteAll (Path dir) throws IOException {
    if (!Files.exists(dir)) return; // our job is already done
    Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
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
}
