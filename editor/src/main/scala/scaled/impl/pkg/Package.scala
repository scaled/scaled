//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl.pkg

import com.google.common.collect.HashMultimap
import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.Files
import java.util.jar.JarFile
import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.collection.{Set => GSet}
import scaled.impl.Filer

// NOTE: package instances are constructed on a background thread
class Package (mgr :PackageManager, val info :PackageInfo) {
  import scala.collection.convert.WrapAsScala._

  /** Loads and returns the class for the major mode named `name`. */
  def major (name :String) :Class[_] = loader.loadClass(majors(name))
  /** Loads and returns the class for the minor mode named `name`. */
  def minor (name :String) :Class[_] = loader.loadClass(minors(name))
  /** Loads and returns the class for the service with class name `name`. */
  def service (name :String) :Class[_] = loader.loadClass(services(name))
  /** Loads and returns all plugin classes with tag `tag`. */
  def plugins (tag :String) :GSet[Class[_]] = plugins.get(tag).map(loader.loadClass)

  val majors = MMap[String,String]() // mode -> classname for this package's major modes
  val minors = MMap[String,String]() // mode -> classname for this package's minor modes
  val services = MMap[String,String]() // svc classname -> impl classname for this package's svcs

  val plugins   = HashMultimap.create[String,String]() // plugin tag -> classname(s)
  val patterns  = HashMultimap.create[String,String]() // major mode -> mode's file patterns
  val interps   = HashMultimap.create[String,String]() // major mode -> mode's interpreters
  val minorTags = HashMultimap.create[String,String]() // tag -> minor mode

  /** The class loader for classes in this package. */
  val loader :ClassLoader =
    // if this is a special built-in package, use our normal class loader, otherwise we end up
    // doubly defining all of our built-in classes which confuses tools like JRebel
    if (info.builtIn) getClass.getClassLoader
    else new URLClassLoader(Array(info.classesDir.toURI.toURL)) {
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
        var loaders = dependLoaders // first try finding the class in our dependencies
        while (!loaders.isEmpty) {
          try return loaders.head.loadClass(name)
          catch {
            case cnfe :ClassNotFoundException => loaders = loaders.tail
          }
        }
        super.findClass(name) // then fall back to looking locally
      }
    }
  private lazy val dependLoaders = info.depends.flatMap(mgr.resolveDepend(info))

  override def toString = String.format(
    "%s [majors=%s, minors=%s, svcs=%s, deps=%s]",
    info.name, majors.keySet, minors.keySet, services, info.depends)

  // our root may be a directory or a jar file, in either case we scan it for class files
  if (info.root.isDirectory) {
    Filer.descendFiles(info.root) { f =>
      val name = f.getName ; val fn = apply(name)
      if (fn != null) fn(name, new ClassReader(Files.readAllBytes(f.toPath)))
    }
  }
  else if (info.root.getName endsWith ".jar") {
    val jfile = new JarFile(info.root)
    val enum = jfile.entries()
    while (enum.hasMoreElements()) {
      val jentry = enum.nextElement() ; val fn = apply(jentry.getName)
      if (fn != null) {
        val in = jfile.getInputStream(jentry)
        fn(jentry.getName, new ClassReader(in))
        in.close
      }
    }
  }
  else throw new IllegalArgumentException("Unsupported package root ${info.root}")

  private def apply (name :String) = {
    if (name endsWith "Mode.class") parseMode
    else if (name endsWith "Service.class") parseService
    else if (name endsWith "Plugin.class") parsePlugin
    else null
  }

  private abstract class Visitor extends ClassVisitor(Opcodes.ASM5) {
    protected var _cname :String = _
    protected val _anns = MMap[String,HashMultimap[String,String]]()

    override def visit (version :Int, access :Int, name :String, signature :String,
                        superName :String, ifcs :Array[String]) {
      _cname = name.replace('/', '.') // TODO: handle inner classes?
    }

    override def visitAnnotation (desc :String, viz :Boolean) = {
      val attrs = HashMultimap.create[String,String]()
      new AnnotationVisitor(Opcodes.ASM5) {
        override def visit (name :String, value :AnyRef) {
          attrs.put(name, value.toString)
        }
        override def visitArray (name :String) = {
          new AnnotationVisitor(Opcodes.ASM5) {
            override def visit (unused :String, value :AnyRef) {
              attrs.put(name, value.toString)
            }
          }
        }
        override def visitEnd () {
          _anns.put(desc, attrs)
        }
      }
    }
  }

  private def parse (viz :Visitor) = (name :String, reader :ClassReader) => try {
    reader.accept(viz, ClassReader.SKIP_CODE|ClassReader.SKIP_DEBUG|ClassReader.SKIP_FRAMES)
  } catch {
    case e :Exception => mgr.app.log("Error parsing package class: $name", e)
  }

  private def parseMode = parse(new Visitor() {
    override def visitEnd () {
      _anns.get("Lscaled/Major;") foreach { attrs =>
        val mode = attrs.get("name").iterator.next
        majors.put(mode, _cname)
        attrs.get("pats") foreach { patterns.put(mode, _) }
        attrs.get("ints") foreach { interps.put(mode, _) }
      }
      _anns.get("Lscaled/Minor;") foreach { attrs =>
        val mode = attrs.get("name").iterator.next
        minors.put(mode, _cname)
        attrs.get("tags") foreach { minorTags.put(_, mode) }
      }
    }
  })

  private def parseService = parse(new Visitor() {
    override def visitEnd () {
      _anns.get("Lscaled/Service;") foreach { attrs =>
        val impl = attrs.get("impl")
        val pre = _cname.substring(0, _cname.lastIndexOf(".")+1)
        services.put(_cname, if (impl.isEmpty) _cname else pre+impl.iterator.next)
      }
    }
  })

  private def parsePlugin = parse(new Visitor() {
    override def visitEnd () {
      _anns.get("Lscaled/Plugin;") foreach { attrs =>
        val tag = attrs.get("tag").iterator.next
        plugins.put(tag, _cname)
      }
    }
  })
}
