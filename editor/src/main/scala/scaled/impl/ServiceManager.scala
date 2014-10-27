//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import scaled._

// this is factored out so that we can do basic service injection in tests without having
// the whole package manager enchilada up and running
class ServiceInjector (val log :Logger, val exec :Executor)
    extends AbstractService with MetaService {

  // these are implemented in ServiceManager
  def metaFile (name: String) :Path = ???
  def service[T] (clazz: Class[T]) :T = ???
  def process[P] (thunk: => P) :Pipe[P] = ???

  def injectInstance[T] (clazz :Class[T], args :List[Any]) :T = {
    def fail (t :Throwable) = throw new InstantiationException(
      s"Unable to inject $clazz [args=$args]").initCause(t)
    try {
      // println(s"Creating instance of ${clazz.getName}")
      val ctor = clazz.getConstructors match {
        case Array(ctor) => ctor
        case ctors       => throw new IllegalArgumentException(
          s"${clazz.getName} must have only one constructor [has=${ctors.mkString(", ")}]")
      }

      // match the args to the ctor parameters; the first arg of the desired type is used and then
      // removed from the arg list, so we can request multiple args of the same type as long as
      // they're in the correct order
      var remargs = SeqBuffer[Any]().append(args).append(stockArgs)
      val params = ctor.getParameterTypes.map { p => remargs.find(p.isInstance) match {
        case Some(arg) => remargs.remove(arg) ; arg
        case None      => resolveService(p)
      }}
      ctor.newInstance(params.asInstanceOf[Array[Object]] :_*).asInstanceOf[T]
    } catch {
      case ite :InvocationTargetException => fail(ite.getCause)
      case t :Throwable => fail(t)
    }
  }

  override def didStartup () {} // unused
  override def willShutdown () {} // unused

  /** Provides stock injectables (e.g. logger and executor). */
  protected def stockArgs :List[AnyRef] = Nil

  /** Resolves `sclass` as a Scaled service and returns its implementation.
    * @throws InstantiationException if no such service exists. */
  protected def resolveService (sclass :Class[_]) :AbstractService =
    throw new InstantiationException(s"Missing implementation: $sclass")
}

class ServiceManager (app :Scaled) extends ServiceInjector(app.logger, app.exec) {

  private var services = Mutable.cacheMap { iclass :Class[_] =>
    injectInstance(iclass.asInstanceOf[Class[AbstractService]], Nil) }

  // we provide MetaService, so stick ourselves in the cache directly; meta!
  services.put(getClass, this)
  // wire the workspace and package managers up directly as well
  services.put(app.pkgMgr.getClass, app.pkgMgr)
  services.put(app.wspMgr.getClass, app.wspMgr)

  // create our plugin manager and manually register it in the cache
  private var pluginMgr = new PluginManager(app)
  services.put(pluginMgr.getClass, pluginMgr)

  // iterates over all known services and resolves any that are marked `autoLoad`; this is called
  // after the editor is fully initialized but before it loads its starting buffers; we can't do
  // this during our constructor because of initialization inter-depends with plugin manager
  def resolveAutoLoads () :Unit = app.pkgMgr.modules foreach autoLoadSvcs
  private def autoLoadSvcs (mm :ModuleMeta) = mm.autoSvcs.map(mm.loadClass).foreach(resolveService)
  // also auto-load services in packages added after startup
  app.pkgMgr.moduleAdded.onValue(autoLoadSvcs)

  override def metaFile (name :String) = app.pkgMgr.metaDir.resolve(name)
  override def service[T] (clazz :Class[T]) = resolveService(clazz).asInstanceOf[T]
  override def process[P] (thunk : => P) = new Plumbing(exec.bgExec, thunk)

  override def resolveService (sclass :Class[_]) = {
    if (!sclass.getName.endsWith("Service")) throw new InstantiationException(
      s"Service classes must be named FooService: $sclass")
    else app.pkgMgr.service(sclass.getName) match {
      case None       => super.resolveService(sclass)
      case Some(impl) => services.get(impl)
    }
  }
  // TODO: when to unload resolved services?

  override protected def stockArgs :List[AnyRef] = List(log, exec, app)
}
