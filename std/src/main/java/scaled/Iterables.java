//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class Iterables {

  static abstract class ImmIterator<E> implements Iterator<E> {
    public void remove () { throw new UnsupportedOperationException(); }
  }

  static final Iterator<Object> EMPTY_ITER = new ImmIterator<Object>() {
    public boolean hasNext() { return false; }
    public Object next () { throw new NoSuchElementException(); }
  };
}
