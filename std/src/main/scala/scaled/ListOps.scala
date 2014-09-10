//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Pimps for [[scaled.List]]. */
final class ListOps[A] (private val list :List[A]) extends AnyVal {

  def ::[B >: A] (b :B) :List[B] = Data.cons(b, list)

  // TODO: all the games and all the puzzles?
}
