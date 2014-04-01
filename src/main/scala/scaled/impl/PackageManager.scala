//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files

import scala.collection.mutable.{ArrayBuffer, Set => MSet, Map => MMap}

import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}

class PackageManager (metaDir :File) {

  /** Resolves the class for the mode named `name`. */
  def mode (name :String) :Option[Class[_]] = modes.get(name).map(_.mode(name))

  /** Resolves the class for the service with classname `name`. */
  def service (name :String) :Option[Class[_]] = services.get(name).map(_.service(name))

  private val pkgsDir = Filer.requireDir(new File(metaDir, "Packages"))
  private val pkgs = MMap[String,Package]()

  private val modes = MMap[String,Package]()
  private val services = MMap[String,Package]()

  // resolve our "built-in" package, which we locate via the classloader
  getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs foreach { url =>
    if ((url.getProtocol == "file") && !(url.getPath endsWith ".jar")) {
      addPackage(new Package("scaled", new File(url.getPath)))
    }
  }

  // TODO: resolve all packages in the pkgsDir

  private def addPackage (pkg :Package) {
    pkgs += (pkg.name -> pkg)
    pkg.modes.keys foreach { m => modes += (m -> pkg) }
    pkg.services foreach { s => services += (s -> pkg) }
    // println(s"Added package $pkg")
  }

  private class Package (val name :String, root :File) {

    /** Loads and returns the class for the mode named `name`. */
    def mode (name :String) :Class[_] = loader.loadClass(modes(name))
    /** Loads and returns the class for the service with class name `name`. */
    def service (name :String) :Class[_] = loader.loadClass(name)

    val modes = MMap[String,String]() // mode -> classname for all this package's modes
    val services = ArrayBuffer[String]() // service classname for all this package's services
    val depends = MSet[String]() // name of all packages on which we depend
    val classes = root // the directory that contains our classes

    /** The class loader for classes in this package. */
    val loader :ClassLoader = new URLClassLoader(Array(classes.toURI.toURL)) {
      override protected def findClass (name :String) :Class[_] = {
        var pkgs = dependPkgs // first try finding the class in our dependencies
        while (!pkgs.isEmpty) {
          try return pkgs.head.loader.loadClass(name)
          catch {
            case cnfe :ClassNotFoundException => pkgs = pkgs.tail
          }
        }
        super.findClass(name) // then fall back to looking locally
      }
    }
    // TODO: make sure dependent packages are resolved sooner?
    private lazy val dependPkgs = depends.map(pkgs).toList

    override def toString = s"$name [modes=${modes.keys}, svcs=$services, deps=$depends]"

    // scan the package directory locating FooMode.class and FooService.class
    Filer.descendFiles(root) { f =>
      try {
        if (f.getName.endsWith("Mode.class")) parseMode(f)
        if (f.getName.endsWith("Service.class")) parseService(f)
      } catch {
        case e :Exception => println("Error parsing $f: $e")
      }
    }

    private abstract class Visitor (aclass :String) extends ClassVisitor(Opcodes.ASM5) {
      private var _cname :String = _
      override def visit (version :Int, access :Int, name :String, signatur :String,
                          superName :String, ifcs :Array[String]) {
        _cname = name.replace('/', '.') // TODO: handle inner classes?
      }

      override def visitAnnotation (desc :String, viz :Boolean) = {
        if (desc != aclass) null
        else {
          onMatch(_cname)
          new AnnotationVisitor(Opcodes.ASM5) {
            override def visit (name :String, value :AnyRef) {
              onAnnotation(_cname, name, value.toString)
            }
            override def visitArray (name :String) = {
              if (name != "deps") null
              else new AnnotationVisitor(Opcodes.ASM5) {
                override def visit (name :String, value :AnyRef) {
                  depends += value.toString
                }
              }
            }
          }
        }
      }

      def onMatch (cname :String) {}
      def onAnnotation (cname :String, aname :String, value :String) :Unit
    }
    private def parse (file :File, viz :Visitor) {
      val r = new ClassReader(Files.readAllBytes(file.toPath))
      r.accept(viz, ClassReader.SKIP_CODE|ClassReader.SKIP_DEBUG|ClassReader.SKIP_FRAMES)
    }

    private def parseMode (file :File) = parse(file, new Visitor("Lscaled/Mode;") {
      override def onAnnotation (cname :String, aname :String, value :String) {
        if (aname == "name") modes += (value.toString -> cname)
        // TODO: put the description somewhere?
      }
    })

    private def parseService (file :File) = parse(file, new Visitor("Lscaled/Service;") {
      override def onMatch (cname :String) {
        services += cname
      }
      override def onAnnotation (cname :String, aname :String, value :String) {
        // TODO: put the description somewhere?
      }
    })
  }
}
