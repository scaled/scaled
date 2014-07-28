//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => JFunction}
import reactual.Signal
import scala.collection.mutable.ArrayBuffer
import scaled._

/** Implements [[PluginService]] and handles notifications when packages are added and removed. */
class PluginManager (app :Scaled) extends AbstractService with PluginService {
  import scala.collection.convert.WrapAsScala._

  private val psets = ArrayBuffer[PluginSetImpl[_]]()

  // we need to know when packages are added and removed
  app.pkgMgr.moduleAdded.onValue { pkg => psets.foreach { _.moduleAdded(pkg) }}
  app.pkgMgr.moduleAdded.onValue { pkg => psets.foreach { _.moduleRemoved(pkg) }}

  class PluginSetImpl[T <: AbstractPlugin] (tag :String, args :List[Any])
      extends PluginSet[T](tag) {

    private val _added = Signal[T]()
    private val _removed = Signal[T]()
    def added = _added
    def removed = _removed

    private val _plugins = ArrayBuffer[T]()
    def plugins = _plugins

    // add ourselves to the plugin sets list
    psets += this
    // start out adding all matching plugins from known package modules
    app.pkgMgr.modules foreach moduleAdded

    def moduleAdded (mod :ModuleMeta) {
      mod.plugins(tag) foreach { pclass =>
        try {
          val p = app.svcMgr.injectInstance(pclass, args).asInstanceOf[T]
          _plugins += p
          _added.emit(p)
        } catch {
          case t :Throwable => app.logger.log(
            s"Failed to instantiate plugin [tag=$tag, class=$pclass, args=$args]", t)
        }
      }
    }

    def moduleRemoved (mod :ModuleMeta) {
      val pclasses = mod.plugins(tag)
      var ii = 0 ; while (ii < _plugins.length) {
        if (pclasses(_plugins(ii).getClass)) {
          val p = _plugins.remove(ii)
          _removed.emit(p)
          p.close()
          ii -= 1
        }
        ii += 1
      }
    }

    override def close () {
      psets -= this
      super.close()
    }
  }

  def didStartup () {} // TODO
  def willShutdown () {} // TODO

  def resolvePlugins[T <: AbstractPlugin] (tag :String, args :List[Any]) :PluginSet[T] =
    new PluginSetImpl[T](tag, args)
}
