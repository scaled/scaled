//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.nio.file.Path
import scaled.util.BufferBuilder

/** Provides the ability to interact with the Scaled Package Manager. */
@Service(name="package", impl="impl.PackageManager",
         desc="Provides the ability to interact with the Scaled Package Manager.")
trait PackageService {

  /** Returns the directory in which the package with source `source` is installed.
    * @throws NoSuchElementException if no such package exists. */
  def installDir (source :String) :Path

  /** Returns the classpath for the package module with source `source`.
    * @throws NoSuchElementException if no such package exists. */
  def classpath (source :String) :Seq[Path]

  /** Adds debugging info on all loaded packages to `bb`. */
  def describePackages (bb :BufferBuilder) :Unit
}
