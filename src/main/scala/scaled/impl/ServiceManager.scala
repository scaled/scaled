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
        // TODO: error handling
        iclass.newInstance.asInstanceOf[AbstractService]
      }
    })

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
