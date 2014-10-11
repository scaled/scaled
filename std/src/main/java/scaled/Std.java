//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.Objects;

/**
 * Provides utility functions and static methods that operate on {@code scaled} data structures.
 */
public class Std {

  /**
   * Returns a {@link Seq} containing {@code elems}. Ownership of {@code elems} is taken by the
   * created seq. Do not retain a reference to it.
   */
  @SafeVarargs @SuppressWarnings("varargs") public static <A> Seq<A> seq (A... elems) {
    return new Seq<A>(elems, elems.length);
  }

  /** Returns a {@link List} containing {@code e0}. */
  public static <A> List<A> list (A e0) { return List.apply(e0); }
  /** Returns a {@link List} containing {@code e0, e1}. */
  public static <A> List<A> list (A e0, A e1) { return List.apply(e0, e1); }
  /** Returns a {@link List} containing {@code e0, e1, e2}. */
  public static <A> List<A> list (A e0, A e1, A e2) { return List.apply(e0, e1, e2); }
  /** Returns a {@link List} containing {@code e0, e1, e2, e3}. */
  public static <A> List<A> list (A e0, A e1, A e2, A e3) { return List.apply(e0, e1, e2, e3); }
  /** Returns a {@link List} containing {@code elems}. */
  @SafeVarargs @SuppressWarnings("varargs")
  public static <A> List<A> list (A e0, A e1, A e2, A e3, A... eN) {
    return List.from(eN).cons(e3).cons(e2).cons(e1).cons(e0);
  }

  /**
   * Returns a new list with {@code elem} consed onto its head. This supports variance in the type
   * of {@code tail} which unfortunately cannot be supported by {@link List#cons}.
   */
  public static <A> List<A> cons (A head, List<? extends A> tail) {
    @SuppressWarnings("unchecked") List<A> casted = (List<A>)tail;
    return tail.cons(head);
  }

  /** Returns a {@link Set} containing {@code e0}. */
  public static <A> Set<A> set (A e0) {
    return Set.<A>builder(1).append(e0).build().toSet(); }
  /** Returns a {@link Set} containing {@code e0, e1}. */
  public static <A> Set<A> set (A e0, A e1) {
    return Set.<A>builder(2).append(e0).append(e1).build().toSet(); }
  /** Returns a {@link Set} containing {@code e0, e1, e2}. */
  public static <A> Set<A> set (A e0, A e1, A e2) {
    return Set.<A>builder(2).append(e0).append(e1).append(e2).build().toSet(); }
  /** Returns a {@link Set} containing {@code e0, e1, e2, e3}. */
  public static <A> Set<A> set (A e0, A e1, A e2, A e3) {
    return Set.<A>builder(2).append(e0).append(e1).append(e2).append(e3).build().toSet(); }

  /**
   * Returns a {@link Set} containing {@code elems}. Ownership of {@code elems} is taken by the
   * created set. Do not retain a reference to it.
   */
  @SafeVarargs @SuppressWarnings("varargs") public static <A> Set<A> set (A... elems) {
    return Set.from(elems);
  }

  /**
   * Returns the concatenation of {@code as} and {@code bs} as a {@link Seq}.
   */
  public static <A> Seq<A> concat (Ordered<A> as, Ordered<A> bs) {
    Seq.Builder<A> sb = Seq.<A>builder(as.size()+bs.size());
    sb.append(as).append(bs);
    return sb.build();
  }

  /**
   * Returns true if {@code a1s} and {@code a2s} are the same size and their elements are pairwise
   * equal, per {@link Objects#equals}.
   */
  public static boolean equals (Ordered<?> a1s, Ordered<?> a2s) {
    if (a1s.size() != a2s.size()) return false;
    Iterator<?> iter1 = a1s.iterator(), iter2 = a2s.iterator();
    while (iter1.hasNext()) if (!Objects.equals(iter1.next(), iter2.next())) return false;
    return true;
  }

  /**
   * Returns a hash code computed from the first {@code size} elements of {@code elems}. This hash
   * code will be equivalent to {@link Arrays#hashCode}, if {@code elems} were of length {@code
   * size}. {@code elems} must not be null.
   */
  public static int hashCode (Object[] elems, int size) {
    int result = 1;
    for (int ii = 0; ii < size; ii++) {
      Object elem = elems[ii];
      result = 31 * result + (elem == null ? 0 : elem.hashCode());
    }
    return result;
  }
}
