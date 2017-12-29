//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.google.common.collect.HashMultimap
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.util.HashMap
import java.util.regex.Pattern
import scaled._
import scaled.pacman._
import scaled.util.BufferBuilder

/** Extends the base package manager with extra info needed by Scaled. */
class PackageManager (log :Logger) extends AbstractService with PackageService {

  /** A signal emitted when a package module is installed. */
  val moduleAdded = Signal[ModuleMeta]()

  /** A signal emitted when a package module is uninstalled. */
  val moduleRemoved = Signal[ModuleMeta]()

  private val pkgRepo = Pacman.repo

  /** Returns the top-level metadata directory. */
  def metaDir :Path = pkgRepo.metaDir

  /** Returns info on all known modules. */
  def modules :Iterable[ModuleMeta] = metas.values

  /** Resolves the class for the mode named `name`. */
  def mode (major :Boolean, name :String) :Option[Class[_]] =
    Option(modeMap(major).get(name)).map(_.apply(name))

  /** Resolves the implementation class for the service with fq classname `name`. */
  def service (name :String) :Option[Class[_]] = Option(serviceMap.get(name)).map(_.apply(name))

  /** Returns the name of all modes provided by all packages. */
  def modes (major :Boolean) :Iterable[String] = modeMap(major).keySet

  /** Detects the major mode that should be used to edit a buffer named `name` and with `line0` as
    * its first line of text. */
  def detectMode (name :String, line0 :String) :String = {
    // checks for -*- mode: somemode -*- on the first or second line
    def fileLocal :Option[String] = None // TODO
    // if the file starts with #!, detects based on "interpreter"
    def interp :Option[String] = line0 match {
      case text if (text startsWith "#!") =>
        // break #!/usr/bin/perl -w into tokens, filtering out known meaningless tokens
        val tokens = text.substring(2).split("[ /]").mkSeq.filterNot(skipToks)
        tokens.map(i => (i, interps.get(i))) collectFirst {
          case (interp, ms) if (!ms.isEmpty) =>
            if (ms.size > 1) log.log("Multiple modes registered for interpreter '$interp': $ms")
            ms.iterator.next
        }
      case _ => None
    }
    // matches the file name against all registered mode regular expressions
    def pattern (name :String) :Option[String] = {
      val ms = patterns collect { case (p, m) if (p.matcher(name).matches()) => m }
      if (ms.size > 1) log.log(s"Multiple modes match buffer name '$name': $ms")
      ms.headOption
    }
    // println(s"Detecting mode for ${name}")
    fileLocal orElse interp orElse pattern(name) getOrElse "text"
  }
  private val skipToks = Set("", "usr", "local", "bin", "env", "opt")

  /** Returns the set of minor modes that should be auto-activated for `tags`. */
  def tagMinorModes (tags :Seq[String]) :Set[String] = tags.flatMap(minorTags.get _).toSet

  override def didStartup () {} // not used
  override def willShutdown () {} // not used

  override def installDir (source :String) = metas.get(Source.parse(source)).mod.root

  override def classpath (source :String) =
    metas.get(Source.parse(source)).mod.depends(pkgRepo.resolver).classpath.toSeq

  override def describePackages (bb :BufferBuilder) {
    val modmetas = modules.filter(_.mod.name != "test").toSeq.sortBy(_.mod.toString)

    bb.addHeader("Packages")
    bb.addKeysValues("Root: " -> metaDir.toString,
                     "Modules: " -> modmetas.size.toString)

    val ignoreModuleJar = Props.ignoreModuleJar
    for (meta <- modmetas) {
      bb.addSubHeader(meta.mod.toString)
      val codeDir = metaDir.relativize(meta.loader.mod.classpath(ignoreModuleJar))
      bb.addKeysValues(Seq("Code: "        -> s"%root%/$codeDir",
                           "Majors: "      -> fmt(meta.majors),
                           "Minors: "      -> fmt(meta.minors),
                           "Services: "    -> fmt(meta.services),
                           "Auto-load: "   -> fmt(meta.autoSvcs),
                           "Plugins: "     -> fmt(meta.plugins.asMap.entrySet),
                           "Patterns: "    -> fmt(meta.patterns.asMap.entrySet),
                           "Interps: "     -> fmt(meta.interps.asMap.entrySet),
                           "Minor tags: "  -> fmt(meta.minorTags.asMap.entrySet)
                           ).filter(_._2 != ""))
    }
  }

  private def fmt (iter :JIterable[_]) :String = iter.mkString(", ")
  private def fmt (iter :scala.Iterable[_]) :String = (iter.map {
    case (k, v) => s"$k=$v"
    case v      => v
  }).mkString(", ")

  private def moduleAdded (mod :Module) {
    // create a package metadata ; there's some special hackery to handle the fact that services
    // are defined in scaled-api and implemented in scaled-editor, which is not normally allowed
    val meta = if (mod.source != ScaledAPI) new ModuleMeta(log, pkgRepo, mod)
               else new ModuleMeta(log, pkgRepo, mod) {
                 override def service (name :String) =
                   metas.get(ScaledEditor).loadClass(services(name))
               }
    metas.put(mod.source, meta)

    // map this package's major and minor modes, services and plugins
    meta.majors.keySet foreach { majorMap.put(_, meta.major _) }
    meta.minors.keySet foreach { minorMap.put(_, meta.minor _) }
    meta.services.keySet foreach { serviceMap.put(_, meta.service _) }
    // map the file patterns and interpreters defined by this package's major modes
    meta.patterns.asMap.toMapV foreach { (m, ps) => ps foreach { p =>
      try patterns += (Pattern.compile(p) -> m)
      catch {
        case e :Exception => log.log(s"Mode $m specified invalid pattern: $p: $e")
      }
    }}
    meta.interps.asMap.toMapV foreach { (m, is) => is foreach { i => interps.put(i, m) }}
    // map the tags defined by this pattern's minor modes
    minorTags.putAll(meta.minorTags)
    // tell any interested parties about this new package module
    PackageManager.this.moduleAdded.emit(meta)
  }

  private def moduleRemoved (mod :Module) {
    // TODO
  }

  private val metas = new HashMap[Source,ModuleMeta]()

  private type Finder = String => Class[_]
  private val serviceMap = new HashMap[String,Finder]()
  private val majorMap = new HashMap[String,Finder]()
  private val minorMap = new HashMap[String,Finder]()
  private def modeMap (major :Boolean) = if (major) majorMap else minorMap

  private val patterns   = SeqBuffer[(Pattern,String)]()
  private val interps    = HashMultimap.create[String,String]()
  private val minorTags  = HashMultimap.create[String,String]()

  private val ScaledAPI = Source.parse("git:https://github.com/scaled/scaled.git#api")
  private val ScaledEditor = Source.parse("git:https://github.com/scaled/scaled.git#editor")

  // reroute Pacman logging to *messages*
  Log.target = new Log.Target() {
    val slog = PackageManager.this.log
    override def log (msg :String) = slog.log(msg)
    override def log (msg :String, exn :Throwable) = slog.log(msg, exn)
  }
  // wire up our observer
  pkgRepo.observer = new PackageRepo.Observer() {
    def packageAdded (pkg :Package) :Unit = pkg.modules.foreach(moduleAdded)
    def packageRemoved (pkg :Package) :Unit = pkg.modules.foreach(moduleRemoved)
  }
  pkgRepo.packages foreach pkgRepo.observer.packageAdded
}
