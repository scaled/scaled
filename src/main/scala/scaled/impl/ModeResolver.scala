//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.Field
import reactual.Future
import scala.collection.mutable.{Map => MMap}
import scaled._

abstract class EnvImpl (val editor :Editor, val view :RBufferView, val disp :Dispatcher) extends Env

abstract class ModeResolver (editor :Editor) {

  /** Returns the names of all known modes, major if `major`, minor if not. */
  def modes (major :Boolean) :Set[String] = Set()

  /** Returns the names of all minor modes with tags that overlap `tags`. */
  def minorModes (tags :Array[String]) :Set[String] = Set()

  /** Resolves and instantiates the major mode `mode` with the supplied environment. */
  def resolveMajor (mode :String, view :BufferViewImpl, disp :DispatcherImpl,
                    args :List[Any]) :Future[MajorMode] =
    locate(true, mode) flatMap(requireMajor(mode)) flatMap(resolve(mode, view, disp, args))

  /** Resolves and instantiates the minor mode `mode` with the supplied environment. */
  def resolveMinor (mode :String, view :BufferViewImpl, disp :DispatcherImpl, major :MajorMode,
                    args :List[Any]) :Future[MinorMode] =
    locate(false, mode) flatMap(requireMinor(mode)) flatMap(resolve(mode, view, disp, major :: args))

  protected def locate (major :Boolean, mode :String) :Future[Class[_]]
  protected def resolveConfig (mode :String, defs :List[Config.Defs]) :Config
  protected def injectInstance[T] (clazz :Class[T], args :List[Any]) :T

  private def requireMajor (mode :String) =
    reqType(classOf[MajorMode], s"$mode is not a major mode.") _
  private def requireMinor (mode :String) =
    reqType(classOf[MinorMode], s"$mode is not a minor mode.") _
  private def reqType[T] (mclass :Class[T], errmsg :String)(clazz :Class[_]) =
    if (mclass.isAssignableFrom(clazz)) Future.success(clazz.asInstanceOf[Class[T]])
    else Future.failure(new IllegalArgumentException(errmsg))

  private def resolve[T] (mode :String, view :BufferViewImpl, disp :DispatcherImpl,
                          args :List[Any])(modeClass :Class[T]) :Future[T] = try {
    val envargs = new EnvImpl(editor, view, disp) {
      def resolveConfig (mode :String, defs :List[Config.Defs]) =
        ModeResolver.this.resolveConfig(mode, defs)
    } :: args
    Future.success(injectInstance(modeClass, envargs))
  } catch {
    case cnfe :ClassNotFoundException => Future.failure(
      new IllegalArgumentException(s"$mode bound to unknown class: $modeClass"))
    case e :Exception => Future.failure(e)
  }
}

class AppModeResolver (app :Main, editor :Editor) extends ModeResolver(editor) {

  override def modes (major :Boolean) = Set() ++ app.pkgMgr.modes(major)
  override def minorModes (tags :Array[String]) = app.pkgMgr.minorModes(tags)

  override protected def locate (major :Boolean, mode :String) =
    app.pkgMgr.mode(major, mode)
  override protected def resolveConfig (mode :String, defs :List[Config.Defs]) =
    app.cfgMgr.resolveConfig(mode, defs)
  override protected def injectInstance[T] (clazz :Class[T], args :List[Any]) =
    app.svcMgr.injectInstance(clazz, args)
}
