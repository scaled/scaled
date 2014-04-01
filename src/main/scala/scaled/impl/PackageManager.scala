//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import java.net.URLClassLoader

import scala.collection.mutable.{ArrayBuffer, Set => MSet}

import org.objectweb.asm.ClassReader

class PackageManager (metaDir :File) {

  val pkgsDir = Filer.requireDir(new File(metaDir, "Packages"))

  // resolve our "built-in" package, which we locate via the classloader
  getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs foreach { url =>
    if ((url.getProtocol == "file") && !(url.getPath endsWith ".jar")) {
      resolvePackage("scaled", new File(url.getPath))
    }
  }

  // TODO: extract these mode mappings via metadata found in the classpath
  val modes = Map(
    "text"         -> "scaled.major.TextMode",
    "mini-read"    -> "scaled.major.MiniReadMode",
    "mini-yesno"   -> "scaled.major.MiniYesNoMode",
    "mini-readopt" -> "scaled.major.MiniReadOptMode",
    "mini-isearch" -> "scaled.major.ISearchMode"
  )

  def mode (name :String) :Option[Class[_]] = {
    modes.get(name).map(Class.forName)
  }

  def service (name :String) :Option[Class[_]] = {
    None
  }

  // private class Package (name :String, root :File) {
  //   val (majorModes, minorModes, services) = {
  //     // scan the package directory locating FooMode.class and FooService.class
  //     val (mfiles, sfiles) = (ArrayBuffer[File](), ArrayBuffer[File]())
  //     Filer.descendFiles(root) { f =>
  //       if (f.getName.endsWith("Mode.class")) mfiles += f
  //       if (f.getName.endsWith("Service.class")) sfiles += f
  //     }

  //     val depends = MSet[String]()

  //     val (majors, minors, services) = (ArrayBuffer[
  //     // now parse the bytecode for these classes to extract and process those which are annotated
  //     // with @Mode and @Service
  //     mfiles foreach { mf =>
  //     }
  //   }
  // }

  private def resolvePackage (name :String, root :File) {
    println(s"TODO, resolve ${root.getAbsolutePath}")
  }
}
