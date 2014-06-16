//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Loads classes for a particular module. A module has two kinds of depends: binary depends, which
 * are private to the package, and will be searched first, and module depends, wherein a Scaled
 * package module depends on another module (possibly in a different package).
 */
public class ModuleLoader extends URLClassLoader {

  public final Source source;
  public final Path classes;
  public final Collection<Path> binaryDeps;
  public final Iterable<ModuleLoader> moduleDeps;

  public ModuleLoader (Source source, Path classes, Collection<Path> binaryDeps,
                       Iterable<ModuleLoader> moduleDeps) {
    super(toURLs(classes, binaryDeps));
    this.source = source;
    this.classes = classes;
    this.binaryDeps = binaryDeps;
    this.moduleDeps = moduleDeps;
  }

  public void accumBinaryDeps (Set<Path> into) {
    into.addAll(binaryDeps);
    for (ModuleLoader dep : moduleDeps) dep.accumBinaryDeps(into);
  }

  public List<Path> classpath () {
    List<Path> cp = new ArrayList<>();
    Set<Source> seen = new HashSet<Source>();
    buildClasspath(cp, seen);
    return cp;
  }

  public void dump (PrintStream out, String indent, Set<Source> seen) {
    if (seen.add(source)) {
      out.println(indent + source);
      out.println(indent + "= " + classes);
      String dindent = indent + "- ";
      for (Path path : binaryDeps) out.println(dindent + path);
      for (ModuleLoader pkg : moduleDeps) pkg.dump(out, dindent, seen);
    } else {
      out.println(indent + "(*) " + source);
    }
  }

  private void buildClasspath (List<Path> cp, Set<Source> seen) {
    if (seen.add(source)) {
      cp.add(classes);
      cp.addAll(binaryDeps);
      for (ModuleLoader dep : moduleDeps) dep.buildClasspath(cp, seen);
    }
  }

  @Override public URL getResource (String path) {
    URL rsrc = super.getResource(path);
    if (rsrc != null) return rsrc;
    for (ModuleLoader loader : moduleDeps) {
      URL drsrc = loader.getResource(path);
      if (drsrc != null) return drsrc;
    }
    return null;
  }

  @Override protected Class<?> findClass (String name) throws ClassNotFoundException {
    // System.err.println("Seeking "+ name +" in "+ source);
    try { return super.findClass(name); }
    catch (ClassNotFoundException cnfe) {} // check our module deps
    for (ModuleLoader loader : moduleDeps) {
      try { return loader.loadClass(name); }
      catch (ClassNotFoundException cnfe) {} // keep going
    }
    throw new ClassNotFoundException(source + " missing dependency: " + name);
  }

  @Override public String toString () {
    return "ModLoader(" + source + ")";
  }

  private static URL[] toURLs (Path classes, Collection<Path> paths) {
    URL[] urls = new URL[1+paths.size()];
    int ii = 0;
    urls[ii++] = toURL(classes);
    for (Path path : paths) urls[ii++] = toURL(path);
    return urls;
  }

  private static URL toURL (Path path) {
    try { return path.toUri().toURL(); }
    catch (MalformedURLException e) { throw new AssertionError(e); }
  }
}
