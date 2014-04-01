//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files

import scala.collection.mutable.{ArrayBuffer, Set => MSet, Map => MMap}

import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}

import scaled.impl.Filer

// NOTE: package instances are constructed on a background thread
class Package (mgr :PackageManager, val info :PackageInfo) {

  /** Loads and returns the class for the mode named `name`. */
  def mode (name :String) :Class[_] = loader.loadClass(modes(name))
  /** Loads and returns the class for the service with class name `name`. */
  def service (name :String) :Class[_] = loader.loadClass(name)

  val modes = MMap[String,String]() // mode -> classname for all this package's modes
  val services = ArrayBuffer[String]() // service classname for all this package's services

  /** The class loader for classes in this package. */
  val loader :ClassLoader = new URLClassLoader(Array(info.classesDir.toURI.toURL)) {
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
  private lazy val dependPkgs = info.depends.map(mgr.pkgs).toList

  override def toString = s"${info.name} [modes=${modes.keys}, svcs=$services, deps=${info.depends}]"

  // scan the package directory locating FooMode.class and FooService.class
  Filer.descendFiles(info.root) { f =>
    try {
      if (f.getName.endsWith("Mode.class")) parseMode(f)
      if (f.getName.endsWith("Service.class")) parseService(f)
    } catch {
      case e :Exception => println("Error parsing $f: $e")
    }
  }

  private abstract class Visitor (aclass :String) extends ClassVisitor(Opcodes.ASM5) {
    private var _cname :String = _
    override def visit (version :Int, access :Int, name :String, signature :String,
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
