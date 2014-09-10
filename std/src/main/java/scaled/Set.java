//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;

public abstract class Set<E> implements Iterable<E> {

  public static class Builder<E> {
    public Builder<E> add (E elem) {
      throw new RuntimeException("TODO");
      // return this;
    }

    public Set<E> build () {
      throw new RuntimeException("TODO");
    }
  }

  public static <E> Builder<E> builder () {
    return new Builder<E>();
  }

  public static <A> Set<A> apply (A... values) {
    throw new RuntimeException("TODO");
  }

  public abstract int size ();

  public abstract boolean contains (E elem);

  public abstract boolean containsAll (Iterable<? extends E> elem);

  public abstract <F extends E> Set<F> add (F elem);

  public abstract <F extends E> Set<F> addAll (Iterable<F> elems);

  public abstract Set<E> remove (E elem);

  public abstract Set<E> removeAll (Iterable<E> elems);

  // public abstract <F extends E> Set<F> union (Set<F> other);
}
