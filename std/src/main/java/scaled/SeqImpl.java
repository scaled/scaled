//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.util.Iterator;
import java.util.NoSuchElementException;

class SeqImpl {

  static final void checkIndex (int index, int max) {
      if (index < 0 || index >= max) throw new IndexOutOfBoundsException(
      index + " not in [0," + max + ")");
  }

  static final Seq<Object> EMPTY = new Seq<Object>() {
    public int size () { return 0; }
    public Object get (int index) { throw new IndexOutOfBoundsException(index + " not in [0,0)"); }
    public Seq<Object> insert (int index, Object elem) {
      checkIndex(index, 0);
      return new Seq1<Object>(elem);
    }
    public Seq<Object> append (Object elem) {
      return new Seq1<Object>(elem);
    }
    public Iterator<Object> iterator () {
      return Iterables.EMPTY_ITER;
    }
  };

  static class Seq1<E> implements Seq<E> {
    private final E elem0;
    public Seq1 (E elem) {
      elem0 = elem;
    }

    public int size () { return 1; }
    public E get (int index) {
      if (index == 0) return elem0;
      else throw new IndexOutOfBoundsException(index + " not in [0, 1)");
    }

    public Seq<E> insert (int index, E elem) {
      switch (index) {
        case 0: return new Seq2<E>(elem, elem0);
        case 1: return new Seq2<E>(elem0, elem);
        default: throw new IndexOutOfBoundsException(index + " not in [0, 1)");
      }
    }

    public Seq<E> append (E elem) {
      return new Seq2<E>(elem0, elem);
    }

    public Iterator<E> iterator () {
      return new Iterables.ImmIterator<E>() {
        private int index = 0;
        public boolean hasNext () {
          return index == 0;
        }
        public E next () {
          if (index == 0) try { return elem0; } finally { index += 1; }
          else throw new NoSuchElementException();
        }
      };
    }
  }

  static class Seq2<E> implements Seq<E> {
    private final E elem0;
    private final E elem1;

    public Seq2 (E elem0, E elem1) {
      this.elem0 = elem0;
      this.elem1 = elem1;
    }

    public int size () { return 2; }
    public E get (int index) {
      switch (index) {
        case 0: return elem0;
        case 1: return elem1;
        default: throw new IndexOutOfBoundsException(index + " not in [0, 2)");
      }
    }

    public Seq<E> insert (int index, E elem) {
      switch (index) {
        case 0: return Data.seq(elem, elem0, elem1);
        case 1: return Data.seq(elem0, elem, elem1);
        case 2: return Data.seq(elem0, elem1, elem);
        default: throw new IndexOutOfBoundsException(index + " not in [0, 2)");
      }
    }

    public Seq<E> append (E elem) {
      return Data.seq(elem0, elem1, elem);
    }

    public Iterator<E> iterator () {
      return new Iterables.ImmIterator<E>() {
        private int index = 0;
        public boolean hasNext () {
          return index < 3;
        }
        public E next () {
          switch (index) {
            case 0: try { return elem0; } finally { index += 1; }
            case 1: try { return elem1; } finally { index += 1; }
            default: throw new NoSuchElementException();
          }
        }
      };
    }
  }

  static class SeqN<E> implements Seq<E> {
    private Object[] elems;

    public SeqN (Object[] elems) {
      this.elems = elems;
    }

    public int size () { return elems.length; }
    public E get (int index) {
      @SuppressWarnings("unchecked") E elem = (E)elems[index];
      return elem;
    }
    public Seq<E> insert (int index, E elem) { throw new RuntimeException("TODO"); }
    public Seq<E> append (E elem) { return insert(size(), elem); }

    public Iterator<E> iterator () {
      return new Iterables.ImmIterator<E>() {
        private int index = 0;
        public boolean hasNext () {
          return index < elems.length;
        }
        public E next () {
          if (index < elems.length) try { return get(index); } finally { index += 1; }
          else throw new NoSuchElementException();
        }
      };
    }
  }
}
