//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import scala.runtime.BoxedUnit;

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

  /** Returns a two-tuple containing {@code a} and {@code b}. */
  public static <A, B> scala.Tuple2<A, B> pair (A a, B b) {
    return new scala.Tuple2<A, B>(a, b);
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

  /** Bridges a {@link Function} to a {@code scala.Function1}.
    * Useful until Scala gets its shit together SAM-wise. */
  public static <T1,R> scala.Function1<T1,R> fn (Function<T1,R> fn) {
    return new scala.runtime.AbstractFunction1<T1,R>() {
      public R apply (T1 value) { return fn.apply(value); }
    };
  }

  /** Bridges a {@link Consumer} to a {@code scala.Function1} which returns {@code Unit}.
    * Useful until Scala gets its shit together SAM-wise. */
  public static <T> scala.Function1<T,BoxedUnit> fnU (Consumer<T> fn) {
    return new scala.runtime.AbstractFunction1<T,BoxedUnit>() {
      public BoxedUnit apply (T value) { fn.accept(value); return BoxedUnit.UNIT; }
    };
  }

  /** Bridges a {@link Supplier} to a {@code scala.Function0}.
    * Useful until Scala gets its shit together SAM-wise. */
  public static <R> scala.Function0<R> fn0 (Supplier<R> fn) {
    return new scala.runtime.AbstractFunction0<R>() {
      public R apply () {
        return fn.get();
      }
    };
  }

  /** Bridges a {@link BiFunction} to a {@code scala.Function2}.
    * Useful until Scala gets its shit together SAM-wise. */
  public static <T1,T2,R> scala.Function2<T1,T2,R> fn2 (BiFunction<T1,T2,R> fn) {
    return new scala.runtime.AbstractFunction2<T1,T2,R>() {
      public R apply (T1 v1, T2 v2) { return fn.apply(v1, v2); }
    };
  }
}
