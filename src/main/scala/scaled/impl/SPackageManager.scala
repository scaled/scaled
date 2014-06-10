//
// Scaled Editor - the Scaled core editor implementation
// http://github.com/scaled/scaled-editor/blob/master/LICENSE

package scaled.impl

import com.google.common.collect.HashMultimap
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.util.regex.Pattern
import reactual.Signal
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled.{AbstractService, Logger, PackageService}
import scaled.pacman._

/** Extends the base package manager with extra info needed by Scaled. */
class SPackageManager (log :Logger) extends AbstractService with PackageService {
  import scala.collection.convert.WrapAsScala._

  /** A signal emitted when a package is installed. */
  val packageAdded = Signal[PackageMeta]()

  /** A signal emitted when a package is uninstalled. */
  val packageRemoved = Signal[PackageMeta]()

  /** Returns the top-level metadata directory. */
  def metaDir :Path = PackageManager.metaDir

  /** Returns info on all known packages. */
  def packages :Iterable[PackageMeta] = metas.values

  /** Resolves the class for the mode named `name`. */
  def mode (major :Boolean, name :String) :Option[Class[_]] =
    modeMap(major).get(name).map(_.apply(name))

  /** Resolves the implementation class for the service with fq classname `name`. */
  def service (name :String) :Option[Class[_]] = serviceMap.get(name).map(_.apply(name))

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
        val tokens = text.substring(2).split("[ /]").filterNot(skipToks)
        tokens.map(i => (i, interps.get(i))) collectFirst {
          case (interp, ms) if (!ms.isEmpty) =>
            if (ms.size > 1) log.log("Multiple modes registered for interpreter '$interp': $ms")
            ms.head
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
  def minorModes (tags :Array[String]) :Set[String] = Set() ++ tags flatMap (minorTags.get _)

  override def didStartup () {} // not used
  override def willShutdown () {} // not used

  override def listPackages = metas.values.map(_.info)

  override def classpath (source :String) = metas(Source.parse(source)).pkg.loader.classpath

  private val metas = MMap[Source,PackageMeta]()

  private type Finder = String => Class[_]
  private val serviceMap = MMap[String,Finder]()
  private val majorMap = MMap[String,Finder]()
  private val minorMap = MMap[String,Finder]()
  private def modeMap (major :Boolean) = if (major) majorMap else minorMap

  private val patterns  = ArrayBuffer[(Pattern,String)]()
  private val interps   = HashMultimap.create[String,String]()
  private val minorTags = HashMultimap.create[String,String]()

  private val ScaledAPI = Source.parse("git:https://github.com/scaled/scaled-api.git")
  private val ScaledEditor = Source.parse("git:https://github.com/scaled/scaled-editor.git")

  // wire up our observer
  PackageManager.observer = new PackageManager.Observer() {
    def packageAdded (pkg :Package) {
      // create a package metadata ; there's some special hackery to handle the fact that services
      // are defined in scaled-api and implemented in scaled-editor, which is not normally allowed
      val meta = if (pkg.source != ScaledAPI) new PackageMeta(log, pkg)
                 else new PackageMeta(log, pkg) {
                   override def service (name :String) =
                     metas(ScaledEditor).pkg.loader.loadClass(services(name))
                 }
      metas.put(pkg.source, meta)

      // map this package's major and minor modes, services and plugins
      meta.majors.keySet foreach { majorMap.put(_, meta.major _) }
      meta.minors.keySet foreach { minorMap.put(_, meta.minor _) }
      meta.services.keySet foreach { serviceMap.put(_, meta.service _) }
      // map the file patterns and interpreters defined by this package's major modes
      meta.patterns.asMap foreach { case (m, ps) => ps foreach { p =>
        try patterns += (Pattern.compile(p) -> m)
        catch {
          case e :Exception => log.log(s"Mode $m specified invalid pattern: $p: $e")
        }
      }}
      meta.interps.asMap foreach { case (m, is) =>
        is foreach { i => interps.put(i, m) }
      }
      // map the tags defined by this pattern's minor modes
      minorTags.putAll(meta.minorTags)
      // tell any interested parties about this new package
      SPackageManager.this.packageAdded.emit(meta)
    }

    def packageRemoved (pkg :Package) {
      // TODO
    }
  }
  PackageManager.packages foreach PackageManager.observer.packageAdded
}
