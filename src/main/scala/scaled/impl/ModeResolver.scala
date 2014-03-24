//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scaled._

class ModeResolver (editor :Editor) {

  // TODO: extract these mode mappings via metadata found in the classpath
  val majorModes = Map(
    "text"       -> "scaled.major.TextMode",
    "mini-read"  -> "scaled.major.MiniReadMode",
    "mini-yesno" -> "scaled.major.MiniYesNoMode"
  )
  val minorModes = Map(
    "todo" -> "todo"
  )

  def resolveMajor (mode :String, config :Config, view :BufferViewImpl, disp :DispatcherImpl,
                    args :List[Any]) :Option[MajorMode] = majorModes.get(mode).map(
    resolveMode(mode, List(editor, config, view.buffer, view, disp) ++ args)).map(_ match {
      case mm :MajorMode => mm
      case mm => throw new IllegalArgumentException(
        s"$mode did not resolve to an instanceof MajorMode. Got $mm")
    })

  def resolveMinor (mode :String, config :Config, view :BufferViewImpl, disp :DispatcherImpl,
                    major :MajorMode, args :List[Any]) :Option[MinorMode] = minorModes.get(mode).map(
    resolveMode(mode, List(editor, config, view.buffer, view, disp, major) ++ args)).map(_ match {
      case mm :MinorMode => mm
      case mm => throw new IllegalArgumentException(
        s"$mode did not resolve to an instanceof MinorMode. Got $mm")
    })

  private def resolveMode (mode :String, args :List[Any])(modeClass :String) :Any = try {
    val clazz = Class.forName(modeClass)
    val ctor = clazz.getConstructors match {
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
    var remargs = args
    val params = ctor.getParameterTypes.map { p =>
      remargs.find(p.isInstance) match {
        case Some(arg) => remargs = minus(remargs, arg) ; arg
        case None => throw new IllegalArgumentException(
          s"Unable to satisfy mode dependency [type=$p, remargs=$remargs]")
      }
    }
    ctor.newInstance(params.asInstanceOf[Array[Object]] :_*)
  } catch {
    case cnfe :ClassNotFoundException =>
      throw new IllegalArgumentException(s"$mode bound to unknown class: $modeClass")
  }
}
