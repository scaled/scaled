//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.google.common.collect.{HashMultimap, Sets}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitOption, FileVisitResult, SimpleFileVisitor, Path}
import java.util.jar.JarFile
import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.collection.{Set => GSet}
import scaled.Logger
import scaled.pacman._

/** Contains additional metadata for a Scaled package module. This metadata is extracted from
  * annotations on the Java bytecode found in the module code. */
class ModuleMeta (log :Logger, repo :PackageRepo, val mod :Module) {
  import scala.collection.convert.WrapAsScala._

  /** Returns the class loader for our module. */
  def loader :ModuleLoader = mod.loader(repo)
  /** Loads the class `name` via this module's class loader. */
  def loadClass (name :String) :Class[_] = loader.loadClass(name)

  /** Loads and returns the class for the major mode named `name`. */
  def major (name :String) :Class[_] = loadClass(majors(name))
  /** Loads and returns the class for the minor mode named `name`. */
  def minor (name :String) :Class[_] = loadClass(minors(name))
  /** Loads and returns the class for the service with class name `name`. */
  def service (name :String) :Class[_] = loadClass(services(name))
  /** Loads and returns all plugin classes with tag `tag`. */
  def plugins (tag :String) :GSet[Class[_]] = plugins.get(tag).map(loadClass)

  val majors = MMap[String,String]() // mode -> classname for this package's major modes
  val minors = MMap[String,String]() // mode -> classname for this package's minor modes
  val services = MMap[String,String]() // svc classname -> impl classname for this package's svcs

  val plugins   = HashMultimap.create[String,String]() // plugin tag -> classname(s)
  val patterns  = HashMultimap.create[String,String]() // major mode -> mode's file patterns
  val interps   = HashMultimap.create[String,String]() // major mode -> mode's interpreters
  val minorTags = HashMultimap.create[String,String]() // tag -> minor mode

  override def toString = String.format(
    "%s [majors=%s, minors=%s, svcs=%s, deps=%s]",
    mod.name, majors.keySet, minors.keySet, services, mod.depends)

  // our root may be a directory or a jar file, in either case we scan it for class files
  if (Files.isDirectory(mod.root)) {
    val opts = Sets.newHashSet(FileVisitOption.FOLLOW_LINKS)
    Files.walkFileTree(mod.root, opts, 32, new SimpleFileVisitor[Path]() {
      override def visitFile (file :Path, attrs :BasicFileAttributes) = {
        if (attrs.isRegularFile) {
          val name = file.getFileName.toString ; val fn = apply(name)
          if (fn != null) fn(name, new ClassReader(Files.readAllBytes(file)))
        }
        FileVisitResult.CONTINUE
      }
    })
  }
  else if (mod.root.getFileName.toString endsWith ".jar") {
    val jfile = new JarFile(mod.root.toFile)
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
  else throw new IllegalArgumentException("Unsupported package root ${mod.root}")

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
    case e :Exception => log.log(s"Error parsing package class: $name", e)
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
