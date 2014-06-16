//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
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

  /** Copies the contents of {@code fromDir} into {@code toDir} recursively.
    * The directory structure under {@code fromDir} is replicated into {@code toDir} as needed. */
  public static void copyAll (Path fromDir, Path toDir) throws IOException {
    Files.walkFileTree(fromDir, new SimpleFileVisitor<Path>() {
      @Override public FileVisitResult preVisitDirectory (Path dir, BasicFileAttributes attrs)
      throws IOException {
        Path targetDir = toDir.resolve(fromDir.relativize(dir));
        if (!Files.exists(targetDir)) Files.createDirectory(targetDir);
        return FileVisitResult.CONTINUE;
      }

      @Override public FileVisitResult visitFile (Path file, BasicFileAttributes attrs)
      throws IOException {
        Files.copy(file, toDir.resolve(fromDir.relativize(file)),
                   StandardCopyOption.REPLACE_EXISTING);
        return FileVisitResult.CONTINUE;
      }
    });
  }
}
