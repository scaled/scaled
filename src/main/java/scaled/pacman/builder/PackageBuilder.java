//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman.builder;

import java.io.IOException;
import scaled.pacman.Package;
import scaled.pacman.*;

/**
 * Handles the compilation of a package's code.
 */
public class PackageBuilder {

  public PackageBuilder (Package pkg) {
    _pkg = pkg;
  }

  /** Cleans out this package's build results directory. */
  public void clean () throws IOException {
    System.err.println("TODO: clean " + _pkg.classesDir());
  }

  /** Builds this package. */
  public void build () throws IOException {
    System.err.println("TODO: build " + _pkg.root);
  }

  private Package _pkg;
}
