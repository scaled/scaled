//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Loads classes for a particular package. A package has two kinds of dependencies: Maven
 * dependencies, which are private to the package, and will be searched first, and Package
 * dependencies, wherein a Scaled package depends on another Scaled package.
 */
public class PackageLoader extends URLClassLoader {

  public final Source source;
  public final List<Path> localDeps;
  public final Iterable<PackageLoader> packageDeps;

  public PackageLoader (Source source, List<Path> localDeps, Iterable<PackageLoader> packageDeps) {
    super(toURLs(localDeps));
    this.source = source;
    this.localDeps = localDeps;
    this.packageDeps = packageDeps;
  }

  public List<Path> classpath () {
    List<Path> cp = new ArrayList<>();
    buildClasspath(cp);
    return cp;
  }

  private void buildClasspath (List<Path> cp) {
    cp.addAll(localDeps);
    for (PackageLoader dep : packageDeps) dep.buildClasspath(cp);
  }

  @Override public URL getResource (String path) {
    URL rsrc = super.getResource(path);
    if (rsrc != null) return rsrc;
    for (PackageLoader loader : packageDeps) {
      URL drsrc = loader.getResource(path);
      if (drsrc != null) return drsrc;
    }
    return null;
  }

  @Override protected Class<?> findClass (String name) throws ClassNotFoundException {
    // System.err.println("Seeking "+ name +" in "+ source);
    try { return super.findClass(name); }
    catch (ClassNotFoundException cnfe) {} // check our package deps
    for (PackageLoader loader : packageDeps) {
      try { return loader.loadClass(name); }
      catch (ClassNotFoundException cnfe) {} // keep going
    }
    throw new ClassNotFoundException(source + " missing dependency: " + name);
  }

  @Override public String toString () {
    return "PkgLoader(" + source + ")";
  }

  private static URL[] toURLs (List<Path> deps) {
    URL[] urls = new URL[deps.size()];
    for (int ii = 0; ii < urls.length; ii++) {
      try { urls[ii] = deps.get(ii).toUri().toURL(); }
      catch (MalformedURLException e) { throw new AssertionError(e); }
    }
    return urls;
  }
}
