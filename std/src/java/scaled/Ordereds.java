//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Operations on {@link Ordered} collections.
 */
public class Ordereds {

  /** Returns a count of elements that match `pred`. */
  public static <A> int count (Ordered<A> as, Predicate<? super A> pred) {
    int count = 0;
    for (A a : as) if (pred.test(a)) count += 1;
    return count;
  }

  /** [java compat] Drops the longest prefix that satisfies `pred`. */
  public static <A> Ordered<A> dropWhile (Ordered<A> as, Predicate<? super A> pred) {
    Ordered.Builder<A> bb = as.newBuilder(4);
    Iterator<A> iter = as.iterator();
    while (iter.hasNext()) {
      A elem = iter.next();
      if (!pred.test(elem)) {
        bb.append(elem);
        bb.append(iter); // this will exhaust the iterator and terminate the loop
      }
    }
    return bb.build();
  }

  /** [java compat] Returns true if this collection contains an element which satisfies `pred`. */
  public static <A> boolean exists (Ordered<A> as, Predicate<? super A> pred) {
    for (A a : as) if (pred.test(a)) return true;
    return false;
  }

  /** [java compat] Returns all elements of this collection which satisfy `pred`. */
  public static <A> Ordered<A> filter (Ordered<A> as, Predicate<? super A> pred) {
    return as.<A>foldBuildJ((b, a) -> { if (pred.test(a)) b.append(a); });
  }

  /** [java compat] Returns all elements of this collection which do not satisfy `pred`. */
  public static <A> Ordered<A> filterNot (Ordered<A> as, Predicate<? super A> pred) {
    return as.foldBuildJ((b, a) -> { if (!pred.test(a)) b.append(a); });
  }

  /** Applies `f` to each element of this list and returns a collection that contains the
    * "concatenation" of the results. */
  public static <A,B> Ordered<B> flatMap (Ordered<A> as,
                                          Function<? super A, ? extends Iterable<B>> f) {
    return as.foldBuildJ((b, a) -> b.append(f.apply(a)));
  }

  /** [java compat] Folds `op` over this collection, from left to right. */
  public static <A,B> B foldLeft (Ordered<A> as, B zero,
                                  BiFunction<? super B, ? super A, ? extends B> op) {
    B acc = zero;
    for (A a : as) acc = op.apply(acc, a);
    return acc;
  }

  /** [java compat] Returns a new seq which contains `f` applied to each of this collection's
    * elements. */
  public static <A,B> Ordered<B> map (Ordered<A> as, Function<? super A, ? extends B> f) {
    return as.foldBuildJ((b, a) -> b.append(f.apply(a)));
  }

  /** Reduces the collection, from left to right, via `op`.
    * @throws NoSuchElementException when called on an empty collection. */
  public static <A> A reduceLeft (Ordered<A> as, BiFunction<? super A,? super A,? extends A> op) {
    Iterator<A> iter = as.iterator();
    A acc = iter.next();
    while (iter.hasNext()) acc = op.apply(acc, iter.next());
    return acc;
  }
}
