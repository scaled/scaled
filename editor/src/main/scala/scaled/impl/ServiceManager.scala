//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scaled._
import com.google.common.cache.{CacheBuilder, CacheLoader}

// this is factored out so that we can do basic service injection in tests without having
// the whole package manager enchilada up and running
class ServiceInjector (log :Logger) extends AbstractService {

  def injectInstance[T] (clazz :Class[T], args :List[Any]) :T = {
    // println(s"Creating instance of ${clazz.getName}")
    val ctor = clazz.getConstructors match {
      case Array(ctor) => ctor
      case ctors       => throw new IllegalArgumentException(
        s"${clazz.getName} must have only one constructor [has=${ctors.mkString(", ")}]")
    }

    // match the args to the ctor parameters; the first arg of the desired type is used and then
    // removed from the arg list, so we can request multiple args of the same type as long as
    // they're in the correct order
    def minus (args :List[Any], elem :Any) :List[Any] = args match {
      case Nil => Nil
      case h :: t => if (elem == h) t else h :: minus(t, elem)
    }
    var remargs = args ++ stockArgs
    val params = ctor.getParameterTypes.map { p =>
      remargs.find(p.isInstance) match {
        case Some(arg) => remargs = minus(remargs, arg) ; arg
        case None      => resolveService(p) getOrElse {
          log.log(s"Unable to satisfy dependency [type=$p, remargs=$remargs]") ; null
        }
      }
    }
    ctor.newInstance(params.asInstanceOf[Array[Object]] :_*).asInstanceOf[T]
  }

  override def didStartup () {} // unused
  override def willShutdown () {} // unused

  /** Provides stock injectables (e.g. logger and executor). */
  protected def stockArgs :List[AnyRef] = Nil

  /** Resolves `sclass` as a Scaled service. If no such service exists, `None` is returned,
    * otherwise the resolved service implementation is returned. */
  protected def resolveService (sclass :Class[_]) :Option[AbstractService] = None
}

class ServiceManager (app :Main) extends ServiceInjector(app.logger) with MetaService {

  private var services = CacheBuilder.newBuilder.build(
    new CacheLoader[Class[_],AbstractService]() {
      def load (iclass :Class[_]) :AbstractService = {
        injectInstance(iclass.asInstanceOf[Class[AbstractService]], Nil)
      }
    })

  // we provide MetaService, so stick ourselves in the cache directly; meta!
  services.put(getClass, this)

  // create our plugin manager and manually register it in the cache
  private var pluginMgr = new PluginManager(app)
  services.put(pluginMgr.getClass, pluginMgr)

  override def log = app.logger
  override def exec = app.exec
  override def metaFile (name :String) = app.metaDir.resolve(name)
  override def service[T] (clazz :Class[T]) :T = resolveService(clazz) match {
    case None => throw new InstantiationException(s"Unknown service $clazz")
    case Some(svc) => svc.asInstanceOf[T]
  }

  override def resolveService (sclass :Class[_]) :Option[AbstractService] = {
    if (!sclass.getName.endsWith("Service")) None
    else app.pkgMgr.service(sclass.getName) map {
      simpl => services.get(simpl)
    }
  }
  // TODO: when to unload resolved services?

  override protected def stockArgs :List[AnyRef] = List(log, exec)
}
