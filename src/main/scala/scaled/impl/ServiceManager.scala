//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scaled._
import com.google.common.cache.{CacheBuilder, CacheLoader}

class ServiceManager (app :Main) {

  private var services = CacheBuilder.newBuilder.build(
    new CacheLoader[Class[_],AbstractService]() {
      def load (iclass :Class[_]) :AbstractService = {
        println(s"Creating $iclass")
        val ctor = iclass.getConstructors match {
          case Array(ctor) => ctor
          case ctors       => throw new IllegalArgumentException(
            s"Service impls must have only one constructor: $iclass [${ctors.mkString(", ")}]")
        }
        val params = ctor.getParameterTypes.map { pclass =>
          resolveService(pclass) match {
            case Some(svc) => svc
            case None =>
              println(s"Unable to satisfy service dependency [type=$pclass]")
              null
          }
        }
        // TODO: catch+handle instantiation errors?
        ctor.newInstance(params.asInstanceOf[Array[Object]] :_*).asInstanceOf[AbstractService]
      }
    })

  // create our plugin manager and manually register it in the cache
  private var pluginMgr = new PluginManager(app)
  services.put(pluginMgr.getClass, pluginMgr)

  /** Resolves `sclass` as a Scaled service. If no such service exists, `None` is returned,
    * otherwise the resolved service implementation is returned. */
  def resolveService (sclass :Class[_]) :Option[AbstractService] = {
    if (!sclass.getName.endsWith("Service")) None
    else app.pkgMgr.service(sclass.getName) map {
      simpl => services.get(simpl)
    }
  }

  // TODO: when to unload
}
