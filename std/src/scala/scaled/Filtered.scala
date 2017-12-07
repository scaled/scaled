//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Represents a filtered view of a collection.
  * Used for integration with Scala for comprehensions. */
trait Filtered[+A] {

  /** Maps the elements of this filtered view via `fn`.
    * Returns a materialized collection, not a lazy view. */
  def map[B] (fn :(A => B)) :Iterable[B]

  /** Maps elements of this filtered view via `fn` and flattens results into a single collection.
    * Returns a materialized collection, not a lazy view. */
  def flatMap[B] (fn :(A => JIterable[B])) :Iterable[B]

  /** Applies `fn` to each element in this filtered view. */
  def foreach[U] (fn :(A => U)) :Unit

  /** Further restricts this filtered view by `pred`. Returns another lazy view. */
  def withFilter (pred :(A => Boolean)) :Filtered[A]
}
