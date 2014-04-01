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

  /** Loads and returns the class for the major mode named `name`. */
  def major (name :String) :Class[_] = loader.loadClass(majors(name))
  /** Loads and returns the class for the minor mode named `name`. */
  def minor (name :String) :Class[_] = loader.loadClass(minors(name))
  /** Loads and returns the class for the service with class name `name`. */
  def service (name :String) :Class[_] = loader.loadClass(name)

  val majors = MMap[String,String]() // mode -> classname for all this package's major modes
  val minors = MMap[String,String]() // mode -> classname for all this package's minor modes
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

  override def toString = String.format("%s [majors=%s, minors=%s, svcs=%s, deps=%s]",
                                        info.name, majors.keys, minors.keys, services, info.depends)

  // scan the package directory locating FooMode.class and FooService.class
  Filer.descendFiles(info.root) { f =>
    try {
      if (f.getName.endsWith("Mode.class")) parseMode(f)
      if (f.getName.endsWith("Service.class")) parseService(f)
    } catch {
      case e :Exception => println("Error parsing $f: $e")
    }
  }

  private abstract class Visitor extends ClassVisitor(Opcodes.ASM5) {
    protected var _cname :String = _
    override def visit (version :Int, access :Int, name :String, signature :String,
                        superName :String, ifcs :Array[String]) {
      _cname = name.replace('/', '.') // TODO: handle inner classes?
    }
  }

  private def parse (file :File, viz :Visitor) {
    val r = new ClassReader(Files.readAllBytes(file.toPath))
    r.accept(viz, ClassReader.SKIP_CODE|ClassReader.SKIP_DEBUG|ClassReader.SKIP_FRAMES)
  }

  private def parseMode (file :File) = parse(file, new Visitor() {
    override def visitAnnotation (desc :String, viz :Boolean) = {
      if (desc == "Lscaled/Major;") new AnnotationVisitor(Opcodes.ASM5) {
        override def visit (name :String, value :AnyRef) {
          if (name == "name") majors += (value.toString -> _cname)
        }
      }
      else if (desc == "Lscaled/Minor;") new AnnotationVisitor(Opcodes.ASM5) {
        override def visit (name :String, value :AnyRef) {
          if (name == "name") minors += (value.toString -> _cname)
        }
      }
      else  null
    }
  })

  private def parseService (file :File) = parse(file, new Visitor() {
    override def visitAnnotation (desc :String, viz :Boolean) = {
      if (desc == "Lscaled/Service;") services += _cname
      null
    }
  })
}
