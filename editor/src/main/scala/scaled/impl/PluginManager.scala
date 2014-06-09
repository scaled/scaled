//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => JFunction}
import reactual.Signal
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.pkg.Package

/** Implements [[PluginService]] and handles notifications when packages are added and removed. */
class PluginManager (app :Main) extends AbstractService with PluginService {
  import scala.collection.convert.WrapAsScala._

  // we need to know when packages are added and removed
  app.pkgMgr.packageAdded.onValue { pkg => psets.values.foreach { _.packageAdded(pkg) }}
  app.pkgMgr.packageAdded.onValue { pkg => psets.values.foreach { _.packageRemoved(pkg) }}

  class PluginSetImpl[T <: AbstractPlugin] (tag :String) extends PluginSet[T](tag) {

    private val _added = Signal[T]()
    private val _removed = Signal[T]()
    def added = _added
    def removed = _removed

    private val _plugins = ArrayBuffer[T]()
    def plugins = _plugins

    // start out adding all matching plugins from known packages
    app.pkgMgr.packages foreach packageAdded

    def packageAdded (pkg :Package) {
      pkg.plugins(tag) foreach { pclass =>
        try {
          val p = pclass.newInstance.asInstanceOf[T]
          _plugins += p
          _added.emit(p)
        } catch {
          case t :Throwable =>
            println(s"Failed to instantiate plugin [tag=$tag, class=$pclass]")
            t.printStackTrace(System.out)
        }
      }
    }

    def packageRemoved (pkg :Package) {
      val pclasses = pkg.plugins(tag)
      var ii = 0 ; while (ii < _plugins.length) {
        if (pclasses(_plugins(ii).getClass)) {
          _removed.emit(_plugins.remove(ii))
          ii -= 1
        }
        ii += 1
      }
    }
  }

  private val psets = new ConcurrentHashMap[String,PluginSetImpl[_]]()
  private val psetComputer = new JFunction[String,PluginSetImpl[_]]() {
    def apply (tag :String) = new PluginSetImpl(tag)
  }

  def didStartup () {} // TODO
  def willShutdown () {} // TODO

  def resolvePlugins[T <: AbstractPlugin] (tag :String) :PluginSet[T] =
    psets.computeIfAbsent(tag, psetComputer).asInstanceOf[PluginSet[T]]
}
