//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File
import java.net.URLClassLoader

import scala.collection.mutable.{Map => MMap}

import scaled.impl._

class PackageManager (app :Main) {

  /** Resolves the class for the mode named `name`. */
  def mode (name :String) :Option[Class[_]] = modes.get(name).map(_.mode(name))

  /** Resolves the class for the service with classname `name`. */
  def service (name :String) :Option[Class[_]] = services.get(name).map(_.service(name))

  /** A mapping from repo URL to package. Repo URL is the unique global identifier for a package, and
    * is what is used to express inter-package dependencies. */
  val pkgs = MMap[String,Package]()

  private val modes = MMap[String,Package]()
  private val services = MMap[String,Package]()

  // resolve our "built-in" package, which we locate via the classloader
  getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs foreach { url =>
    if ((url.getProtocol == "file") && !(url.getPath endsWith ".jar")) {
      // resolve this package immediately, on the main thread because we need our basic modes to be
      // available immediately; TODO: really? maybe we could defer those as well?
      addPackage(new Package(this, PackageInfo.builtin(new File(url.getPath))))
    }
  }

  // resolve all packages in our packages directory
  private val pkgsDir = Filer.requireDir(new File(app.metaDir, "Packages"))
  Filer.descendDirs(pkgsDir) { dir =>
    val pkgFile = new File(dir, "package.scaled")
    if (!pkgFile.exists) true // descend into subdirs
    else {
      app.exec.execute(new Runnable() {
        override def run () = resolvePackage(PackageInfo(pkgFile))
      })
      false // stop descending
    }
  }

  private def addPackage (pkg :Package) {
    pkgs += (pkg.info.repo -> pkg)
    pkg.modes.keys foreach { m => modes += (m -> pkg) }
    pkg.services foreach { s => services += (s -> pkg) }
    // println(s"Added package $pkg")
  }

  // NOTE: this is called on a background thread
  private def resolvePackage (info :PackageInfo) {
    // make sure this package is up to date
    println(s"Resolving package $info")
    // create the package and scan its classes
    val pkg = new Package(this, info)
    // lastly map this package's information back on our main thread
    onMainThread { addPackage(pkg) }
  }
}
