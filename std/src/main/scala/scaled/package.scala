//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

import scala.language.implicitConversions

package object scaled {

  def Nil[A] :List[A] = Data.nil()

  // TODO: list(...) seq(...) and other constructors

  implicit def listOps[A] (list :List[A]) :ListOps[A] = new ListOps[A](list)
}
