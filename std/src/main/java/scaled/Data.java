//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

public class Data {

  /** Returns the empty list. */
  public static <A> List<A> nil () {
    @SuppressWarnings("unchecked") List<A> nil = (List<A>)ListImpl.NIL;
    return nil;
  }

  /** Returns a new list with {@code elem} consed onto its head. */
  public static <A> List<A> cons (A head, List<? extends A> tail) {
    @SuppressWarnings("unchecked") List<A> casted = (List<A>)tail;
    return new ListImpl.Cons<A>(head, casted);
  }

  /** Creates a list that contains {@code value}. */
  public static <A> List<A> list (A value) {
    return cons(value, nil());
  }

  /** Creates a list that contains {@code [a, b]}. */
  public static <A> List<A> list (A a, A b) {
    return cons(a, cons(b, nil()));
  }

  /** Creates a list that contains {@code [a, b, c]}. */
  public static <A> List<A> list (A a, A b, A c) {
    return cons(a, cons(b, cons(c, nil())));
  }

  /** Creates a list that contains {@code values}. */
  @SafeVarargs public static <A> List<A> list (A... values) {
    List<A> list = nil();
    for (int ii = values.length-1; ii >= 0; ii -= 1) list = list.cons(values[ii]);
    return list;
  }

  public static <E> Seq<E> seq () {
    @SuppressWarnings("unchecked") Seq<E> empty = (Seq<E>)SeqImpl.EMPTY;
    return empty;
  }

  public static <E> Seq<E> seq (E elem) {
    return new SeqImpl.Seq1<E>(elem);
  }

  public static <E> Seq<E> seq (E elem0, E elem1) {
    return new SeqImpl.Seq2<E>(elem0, elem1);
  }

  @SafeVarargs public static <E> Seq<E> seq (E... elems) {
    @SuppressWarnings("varargs") Object[] es = elems;
    return new SeqImpl.SeqN<E>(es);
  }
}
