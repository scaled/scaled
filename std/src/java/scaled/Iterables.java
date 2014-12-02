//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Function;
import java.lang.Iterable;

public class Iterables {

  /**
   * Returns true if the elements in {@code a1s} and {@code a2s} are pairwise equal, per {@link
   * Objects#equals}. If either iterable contains more elements than the other, they are not equal.
   */
  public static boolean equals (Iterable<?> a1s, Iterable<?> a2s) {
    Iterator<?> iter1 = a1s.iterator(), iter2 = a2s.iterator();
    while (iter1.hasNext()) {
      if (!iter2.hasNext() || !Objects.equals(iter1.next(), iter2.next())) return false;
    }
    return !iter2.hasNext();
  }

  /**
   * Skips {@code count} elements of {@code iter}. If the iterator is exhausted before count
   * elements could be skipped, the skipping is stopped.
   * @return {@code iter} for call chaining.
   */
  public static <A> Iterator<A> skip (Iterator<A> iter, int count) {
    for (int skipped = 0; skipped < count && iter.hasNext(); skipped += 1) iter.next();
    return iter;
  }

  /**
   * Copies the slice {@code [start,end)} of {@code as} into {@code target} at {@code offset}.
   */
  public static void copyInto (Iterable<?> as, int start, int end, Object[] target, int offset) {
    Iterator<?> iter = skip(as.iterator(), start);
    for (int ll = offset+end-start, ii = offset; ii < ll; ii++) target[ii] = iter.next();
  }

  static final Iterator<Object> EMPTY_ITER = new Iterator<Object>() {
    public boolean hasNext() { return false; }
    public Object next () { throw new NoSuchElementException(); }
  };
}
