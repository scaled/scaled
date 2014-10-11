//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Represents a filtered view of an [[Ordered]] collection. This is used by Scala's for
  * comprehensions, but can also be used manually if one wants to opt-into laziness for a
  * filter+map, filter+foreach, etc.
  */
abstract class Filtered[+A] (source :Ordered[A], pred :(A => Boolean)) {

  /** Maps the elements of this filtered view via `fn`.
    * Returns a materialized collection, not a lazy view. */
  def map[B] (fn :(A => B)) :Ordered[B] = {
    val bb = newBuilder[B]()
    val iter = source.iterator() ; while (iter.hasNext) {
      val elem = iter.next
      if (pred(elem)) bb += fn(elem)
    }
    bb.build()
  }

  /** Maps elements of this filtered view via `fn` and flattens results into a single collection.
    * Returns a materialized collection, not a lazy view. */
  def flatMap[B] (fn :(A => JIterable[B])) :Ordered[B] = {
    val bb = newBuilder[B]()
    val iter = source.iterator() ; while (iter.hasNext) {
      val elem = iter.next
      if (pred(elem)) bb ++= fn(elem)
    }
    bb.build()
  }

  /** Applies `fn` to each element in this filtered view. */
  def foreach[U] (fn :(A => U)) {
    val iter = source.iterator() ; while (iter.hasNext) {
      val elem = iter.next
      if (pred(elem)) fn(elem)
    }
  }

  /** Further restricts this filtered view by `pred`. Returns another lazy view. */
  def withFilter (pred :(A => Boolean)) :Filtered[A] = {
    val outer = this
    new Filtered[A](source, a => this.pred(a) && pred(a)) {
      override protected def newBuilder[B] () = outer.newBuilder[B]()
    }
  }

  protected def newBuilder[B] () :Ordered.Builder[B]
}
