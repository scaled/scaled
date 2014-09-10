//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

/**
 * Represents a collection with a countable number of elements.
 */
public interface Countable<E> extends Iterable<E> {

  /** Returns the number of elements in this countable collection. */
  int size ();
}
