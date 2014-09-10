//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

public interface Seq<E> extends Countable<E> {

  E get (int index);
}
