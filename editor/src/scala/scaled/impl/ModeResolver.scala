//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.Field
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.Errors

abstract class ModeResolver (msvc :MetaService, window :Window, frame :Window#Frame) {

  /** Returns the names of all known modes, major if `major`, minor if not. */
  def modes (major :Boolean) :Set[String] = Set()

  /** Returns the names of all minor modes that match `tags`. */
  def tagMinorModes (tags :Seq[String]) :Set[String] = Set()

  /** Returns the names of all minor modes that match `stateTypes`. */
  def stateMinorModes (stateTypes :Set[Class[_]]) :Set[String] = Set()

  /** Resolves and instantiates the major mode `mode` with the supplied environment. */
  def resolveMajor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    args :List[Any]) :MajorMode =
    resolve(mode, view, mline, disp, args, requireMajor(mode))

  /** Resolves and instantiates the minor mode `mode` with the supplied environment. */
  def resolveMinor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    major :MajorMode, args :List[Any]) :MinorMode =
    resolve(mode, view, mline, disp, major :: args, requireMinor(mode))

  protected def locate (major :Boolean, mode :String) :Class[_]
  protected def configScope :Config.Scope
  protected def injectInstance[T] (clazz :Class[T], args :List[Any]) :T

  private def requireMajor (mode :String) = reqType(mode, classOf[MajorMode])
  private def requireMinor (mode :String) = reqType(mode, classOf[MinorMode])
  private def reqType[T] (mode :String, mclass :Class[T]) = {
    val isMajor = mclass == classOf[MajorMode]
    val clazz = locate(isMajor, mode)
    if (mclass.isAssignableFrom(clazz)) clazz.asInstanceOf[Class[T]]
    else throw new IllegalArgumentException(s"$mode ($clazz) is not a ${mclass.getSimpleName}.")
  }

  private def resolve[T] (mode :String, vw :BufferViewImpl, mln :ModeLine, dsp :DispatcherImpl,
                          args :List[Any], modeClass :Class[T]) :T = {
    val envargs = new Env {
      val msvc = ModeResolver.this.msvc
      val frame = ModeResolver.this.frame
      val window = ModeResolver.this.window
      val view = vw
      val mline = mln
      val disp = dsp
      def configScope = ModeResolver.this.configScope
    } :: args
    injectInstance(modeClass, envargs)
  }
}
