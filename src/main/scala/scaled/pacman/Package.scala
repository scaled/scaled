//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.pacman

/** Package related bits. */
object Package {

  /** Used to identify packages. */
  trait Id {
  }

  /** Used to report dependency trees. */
  case class Node (id :Id, depends :List[Node]) {
    def dump (indent :String = "") {
      println(indent + id)
      depends.foreach(_.dump(indent + "."))
    }
  }
}

/** Contains runtime metadata for an installed package. */
class Package (val info :PackageInfo) {

  /** The loader for classes in this package. */
  val loader :PackageLoader = new PackageLoader(info.source, info.classesDir) {
    override protected def resolveDependLoaders =
      info.depends.flatMap(PackageManager.resolveDepend(info))
  }

  override def toString = info.name
}
