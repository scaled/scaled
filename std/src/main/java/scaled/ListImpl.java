//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.NoSuchElementException;

class ListImpl {

  static final Nil NIL = new Nil();

  static class Nil implements List<Object> {
    @Override public Iterator<Object> iterator () { return Iterables.EMPTY_ITER; }
    @Override public int size () { return 0; }
    @Override public Object head () { throw new NoSuchElementException("nil.head()"); }
    @Override public List<Object> tail () { throw new NoSuchElementException("nil.tail()"); }
    @Override public List<Object> cons (Object elem) { return new Cons<Object>(elem, this); }
  };

  static class Cons<E> implements List<E> {
    @Override public Iterator<E> iterator () {
      return new Iterables.ImmIterator<E>() {
        private List<E> cur;
        @Override public boolean hasNext() {
          return cur != NIL;
        }
        @Override public E next () {
          List<E> cur = this.cur;
          this.cur = cur.tail();
          return cur.head();
        }
      };
    }

    @Override public int size () {
      int size = 0;
      for (List<E> list = this, nil = Data.nil(); list != nil; list = list.tail()) {
        size += 1;
      }
      return size;
    }

    @Override public E head () { return head; }
    @Override public List<E> tail () { return tail; }
    @Override public List<E> cons (E elem) { return new Cons<E>(elem, this); }

    public Cons (E head, List<E> tail) {
      if (tail == null) throw new IllegalArgumentException("List tail must not be null.");
      this.head = head;
      this.tail = tail;
    }

    private final E head;
    private final List<E> tail;
  }

}
