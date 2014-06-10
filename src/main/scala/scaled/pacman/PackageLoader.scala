//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman

import java.net.{URL, URLClassLoader}
import java.nio.file.Path

abstract class PackageLoader (val id :String, val path :Path)
    extends URLClassLoader(Array(path.toUri.toURL)) {

  override def getResource (path :String) :URL = {
    var loaders = dependLoaders // first try finding the resource in our dependencies
    while (!loaders.isEmpty) {
      val r = loaders.head.getResource(path)
      if (r != null) return r
      loaders = loaders.tail
    }
    super.getResource(path)
  }
  override protected def findClass (name :String) :Class[_] = {
    // println(s"Seeking $name in ${info.name}")
    var loaders = dependLoaders // first try finding the class in our dependencies
    while (!loaders.isEmpty) {
      try return loaders.head.loadClass(name)
      catch {
        case cnfe :ClassNotFoundException => loaders = loaders.tail
      }
    }
    try super.findClass(name) // then fall back to looking locally
    catch {
      case cnfe :ClassNotFoundException => // provide a more useful error message
        throw new ClassNotFoundException(s"$id missing dependency: $name")
    }
  }

  override def toString = s"PkgLoader($id)"

  private lazy val dependLoaders :List[ClassLoader] = resolveDependLoaders
  protected def resolveDependLoaders :List[ClassLoader]
}
