//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.lang.reflect.Method
import scala.collection.mutable.{Map => MMap}
import scaled._

/** A single fn-binding.
  * @param mode the mode instance from whence this binding came.
  * @param name the de-camel-cased name of this binding.
  * @param meth the method to which the name is bound.
  * @param wantsTyped whether the fn binding wants to be passed the typed character.
  */
case class FnBinding (mode :Mode, meth :Method, wantsTyped :Boolean) {

  /** Returns the de-camel-cased name of the fn. */
  val name :String = Config.deCamelCase(meth.getName)

  /** Returns a description of the fn. */
  val descrip :String = meth.getAnnotation(classOf[Fn]).value.replaceAll("\\n\\s+", " ")

  /** Invokes this fn binding in response to a key press.
    *
    * @param typed the typed character(s) if this fn is being invoked as a result of a key typed
    * event because no fn binding was found for the key pressed event that preceded it, or null if
    * the fn binding is being invoked as a result of a key pressed event.
    * @return the return value of the fn itself; usually null but fns are allowed to return
    * `false` to indicate that they did not handle the key and any other fns bound to that key
    * should be given a chance to run.
    */
  def invoke (typed :String) :Any = if (wantsTyped) meth.invoke(mode, typed) else meth.invoke(mode)

  override def toString = s"[mode=${mode.name}, name=$name]"
}

/** [[FnBindings]] helper methods. */
object FnBindings {

  /** Creates an `FnBinding` for `mode`'s method `meth`. Reports an error and returns `None` if the
    * method does not conform to a valid fn method signature (i.e. `()` or `(String)`). */
  def toFnBinding (mode :Mode, errFn :(String => Unit))(meth :Method) :Option[FnBinding] = {
    val pcount = meth.getParameterCount
    val wantsTyped = pcount == 1 && meth.getParameterTypes.apply(0) == classOf[String]
    if (!wantsTyped && pcount != 0) {
      errFn(s"Invalid fn method definition $meth. Must take () or (String).")
      None
    } else Some(FnBinding(mode, meth, wantsTyped))
  }
}

/** Resolves all fns defined in `mode` and exposes them via a civilized API. */
class FnBindings (mode :Mode, errFn :(String => Unit)) {

  /** Bindings to the fns exported by this mode. */
  val bindings :Seq[FnBinding] = extractBindings(mode.getClass)

  /** Returns the binding with the specified name, or `None`. */
  def binding (name :String) :Option[FnBinding] = _bindmap.get(name)

  private[this] val _bindmap = bindings.mapBy(_.name)

  private def extractBindings (clazz :Class[_]) :Seq[FnBinding] = {
    val bs = clazz.getDeclaredMethods.mkSeq.filter(_.getAnnotation(classOf[Fn]) != null).flatMap(
      FnBindings.toFnBinding(mode, errFn))
    if (clazz.getSuperclass != null) extractBindings(clazz.getSuperclass) ++ bs
    else bs
  }
}
