//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.{InvocationTargetException, Method}

import scala.collection.mutable.{Map => MMap}

import scaled._

/** A single fn-binding.
  * @param mode the mode instance from whence this binding came.
  * @param name the de-camel-cased name of this binding.
  * @param meth the method to which the name is bound.
  */
case class FnBinding (mode :Mode, name :String, meth :Method) {
  /** Invokes this fn binding. */
  def invoke () = try meth.invoke(mode) catch {
    case e :InvocationTargetException => // TODO: better error reporting
      System.err.println(s"FnBinding choked [mode=${mode.name}, name=$name]")
      e.getCause.printStackTrace(System.err)
  }
}

/** [[FnBindings]] helper methods. */
object FnBindings {

  /** Converts `name` from camelCase to words-separated-by-dashes. */
  def deCamelCase (name :String) = {
    val buf = new StringBuilder(name)
    var ii = 0
    while (ii < buf.length) {
      val c = buf.charAt(ii)
      if (Character.isUpperCase(c)) {
        buf.deleteCharAt(ii)
        buf.insert(ii, '-')
        ii += 1
        buf.insert(ii, Character.toLowerCase(c))
      }
      ii += 1
    }
    buf.toString
  }
}

/** Resolves all fns defined in `mode` and exposes them via a civilized API. */
class FnBindings (mode :Mode) {

  /** Bindings to the fns exported by this mode. */
  val bindings :Seq[FnBinding] = extractBindings(mode.getClass)

  private[this] val _bindmap = bindings map(b => (b.name -> b)) toMap

  /** Returns the binding with the specified name, or `None`. */
  def binding (name :String) :Option[FnBinding] = _bindmap.get(name)

  private def extractBindings (clazz :Class[_]) :Seq[FnBinding] = {
    def toBinding (meth :Method) = FnBinding(mode, FnBindings.deCamelCase(meth.getName), meth)
    val bs = clazz.getDeclaredMethods filter(_.getAnnotation(classOf[Fn]) != null) map(toBinding)
    if (clazz.getSuperclass != null) extractBindings(clazz.getSuperclass) ++ bs
    else bs
  }
}
