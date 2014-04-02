//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.Field
import reactual.Future
import scala.collection.mutable.{Map => MMap}
import scaled._

abstract class ModeResolver (editor :Editor, edconfig :ConfigImpl) {

  def complete (major :Boolean, namePre :String) :Set[String] = Set()

  def minorModes (tags :Array[String]) :Set[String] = Set()

  def resolveMajor (mode :String, view :BufferViewImpl, disp :DispatcherImpl,
                    args :List[Any]) :Future[MajorMode] =
    locate(true, mode) flatMap(requireMajor(mode)) flatMap(resolve(mode, view, disp, args))

  def resolveMinor (mode :String, view :BufferViewImpl, disp :DispatcherImpl, major :MajorMode,
                    args :List[Any]) :Future[MinorMode] =
    locate(false, mode) flatMap(requireMinor(mode)) flatMap(resolve(mode, view, disp, major :: args))

  protected def locate (major :Boolean, mode :String) :Future[Class[_]]

  private def requireMajor (mode :String) =
    reqType(classOf[MajorMode], s"$mode is not a major mode.") _
  private def requireMinor (mode :String) =
    reqType(classOf[MinorMode], s"$mode is not a minor mode.") _
  private def reqType[T] (mclass :Class[T], errmsg :String)(clazz :Class[_]) =
    if (mclass.isAssignableFrom(clazz)) Future.success(clazz.asInstanceOf[Class[T]])
    else Future.failure(new IllegalArgumentException(errmsg))

  /** Resolved config instances for each mode. */
  private val _configs = MMap[String,ConfigImpl]()

  private def resolve[T] (mode :String, view :BufferViewImpl, disp :DispatcherImpl,
                          args :List[Any])(modeClass :Class[T]) :Future[T] = try {
    val ctor = modeClass.getConstructors match {
      case Array(ctor) => ctor
      case ctors       => throw new IllegalArgumentException(
        s"Modes must have only one constructor: $modeClass [${ctors.mkString(", ")}]")
    }

    // match the args to the ctor parameters; the first arg of the desired type is used and then
    // removed from the arg list, so we can request multiple args of the same type as long as
    // they're in the 'correct' order
    def minus (args :List[Any], elem :Any) :List[Any] = args match {
      case Nil => Nil
      case h :: t => if (elem == h) t else h :: minus(t, elem)
    }
    val config = _configs.getOrElseUpdate(mode, new ConfigImpl(mode, Some(edconfig)))
    var remargs = editor :: config :: view.buffer :: view :: disp :: args
    val params = ctor.getParameterTypes.map { p =>
      remargs.find(p.isInstance) match {
        case Some(arg) => remargs = minus(remargs, arg) ; arg
        case None => throw new IllegalArgumentException(
          s"Unable to satisfy mode dependency [type=$p, remargs=$remargs]")
      }
    }
    Future.success(ctor.newInstance(params.asInstanceOf[Array[Object]] :_*).asInstanceOf[T])

  } catch {
    case cnfe :ClassNotFoundException => Future.failure(
      new IllegalArgumentException(s"$mode bound to unknown class: $modeClass"))
    case e :Exception => Future.failure(e)
  }
}

class PackageModeResolver (pmgr :pkg.PackageManager, editor :Editor, edconfig :ConfigImpl)
    extends ModeResolver(editor, edconfig) {

  override def complete (major :Boolean, namePre :String) =
    Set() ++ pmgr.modes(major).filter(_ startsWith namePre)
  override def minorModes (tags :Array[String]) = pmgr.minorModes(tags)
  override protected def locate (major :Boolean, mode :String) = pmgr.mode(major, mode)
}
