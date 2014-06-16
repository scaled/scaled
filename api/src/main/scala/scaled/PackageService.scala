//
// Scaled API - the API against which Scaled extensions are written
// http://github.com/scaled/scaled-api/blob/master/LICENSE

package scaled

import java.nio.file.Path

/** Provides the ability to interact with the Scaled Package Manager. */
@Service(name="package", impl="impl.PackageManager",
         desc="Provides the ability to interact with the Scaled Package Manager.")
trait PackageService {

  /** Returns the classpath for the package module with source `source`.
    * @throws NoSuchElementException if no such package exists. */
  def classpath (source :String) :Seq[Path]
}
