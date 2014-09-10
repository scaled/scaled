//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

/**
 * A simple functional list class, built from cons cells as God and John McCarthy intended.
 */
public interface List<E> extends Countable<E> {

  /**
   * Returns the head of this list.
   * @throws NoSuchElementException if called on the {@code nil} list.
   */
  E head ();

  /**
   * Returns the tail of this list.
   * @throws NoSuchElementException if called on the {@code nil} list.
   */
  List<E> tail ();

  /**
   * Returns a new list with {@code elem} consed onto its head. Note: due to limitations of Java's
   * type system, this method is invariant. Use the static {@code List.cons} if you need the proper
   * contravariance.
   */
  List<E> cons (E elem);
}
