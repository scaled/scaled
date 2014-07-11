//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.Field
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.Errors

abstract class EnvImpl (
  val log  :Logger,      val exec  :Executor, val editor :Editor,
  val view :RBufferView, val mline :ModeLine, val disp   :Dispatcher) extends Env

abstract class ModeResolver (log :Logger, exec :Executor, editor :Editor) {

  /** Returns the names of all known modes, major if `major`, minor if not. */
  def modes (major :Boolean) :Set[String] = Set()

  /** Returns the names of all minor modes with tags that overlap `tags`. */
  def minorModes (tags :Array[String]) :Set[String] = Set()

  /** Resolves and instantiates the major mode `mode` with the supplied environment. */
  def resolveMajor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    args :List[Any]) :MajorMode =
    resolve(mode, view, mline, disp, args, requireMajor(mode))

  /** Resolves and instantiates the minor mode `mode` with the supplied environment. */
  def resolveMinor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    major :MajorMode, args :List[Any]) :MinorMode =
    resolve(mode, view, mline, disp, major :: args, requireMinor(mode))

  protected def locate (major :Boolean, mode :String) :Class[_]
  protected def resolveConfig (mode :String, defs :List[Config.Defs]) :Config
  protected def injectInstance[T] (clazz :Class[T], args :List[Any]) :T

  private def requireMajor (mode :String) = reqType(mode, classOf[MajorMode])
  private def requireMinor (mode :String) = reqType(mode, classOf[MinorMode])
  private def reqType[T] (mode :String, mclass :Class[T]) = {
    val isMajor = mclass == classOf[MajorMode]
    val clazz = locate(isMajor, mode)
    if (mclass.isAssignableFrom(clazz)) clazz.asInstanceOf[Class[T]]
    else throw new IllegalArgumentException(s"$mode ($clazz) is not a ${mclass.getSimpleName}.")
  }

  private def resolve[T] (mode :String, view :BufferViewImpl, mline :ModeLine,
                          disp :DispatcherImpl, args :List[Any], modeClass :Class[T]) :T = {
    val envargs = new EnvImpl(log, exec, editor, view, mline, disp) {
      def resolveConfig (mode :String, defs :List[Config.Defs]) =
        ModeResolver.this.resolveConfig(mode, defs)
    } :: args
    injectInstance(modeClass, envargs)
  }
}

class AppModeResolver (ws :WorkspaceImpl, editor :Editor)
    extends ModeResolver(ws.app.logger, ws.app.exec, editor) {

  override def modes (major :Boolean) = Set() ++ ws.app.pkgMgr.modes(major)
  override def minorModes (tags :Array[String]) = ws.app.pkgMgr.minorModes(tags)

  override protected def locate (major :Boolean, mode :String) =
    ws.app.pkgMgr.mode(major, mode) match {
      case Some(mode) => mode
      case None       => throw Errors.feedback(s"Unknown mode: $mode")
    }
  override protected def resolveConfig (mode :String, defs :List[Config.Defs]) =
    ws.cfgMgr.resolveConfig(mode, defs)
  override protected def injectInstance[T] (clazz :Class[T], args :List[Any]) =
    ws.app.svcMgr.injectInstance(clazz, args)
}
