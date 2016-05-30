//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scaled._

/** Contains [[AutoCloseable]]-related helpers. */
object Close {

  /** Maintains a bag of [[AutoCloseable]]s and simplifies the process of closing them all at once.
    * Note: this class is not thread-safe.
    */
  class Bag extends AutoCloseable {

    private val _acs = SeqBuffer[AutoCloseable]()

    /** Returns true if we have zero closeables in our list. */
    def isEmpty :Boolean = _acs.isEmpty

    /** Adds `ac` to this close list. */
    def add (ac :AutoCloseable) :Unit = _acs += ac
    /** Adds `ac` to this close list. */
    def += (ac :AutoCloseable) :Unit = add(ac)

    /** Removes `ac` from this close list. */
    def remove (ac :AutoCloseable) :Unit = _acs -= ac
    /** Removes `ac` from this close list. */
    def -= (ac :AutoCloseable) :Unit = remove(ac)

    /** Applies `fn` to all closeables in this bag. For sneaky business. */
    def foreach[U] (fn :AutoCloseable => U) :Unit = _acs foreach fn

    /** Closes all closeables in this list and clears it. If any exceptions occur during closure,
      * they are accumulated into single [[RuntimeException]] as suppressed exceptions. */
    def close () {
      var exn = null :RuntimeException
      val iter = _acs.iterator ; while (iter.hasNext) {
        try iter.next.close()
        catch {
          case t :Throwable =>
            if (exn == null) exn = new RuntimeException("Close failure(s).")
            exn.addSuppressed(t)
        }
      }
      _acs.clear()
      if (exn != null) throw exn
    }
  }

  /** Creates a close bag. */
  def bag () = new Bag()

  /** A reference to an object that is created on demand and nulled out when the ref is closed.
    * The referent is not itself closed, use [[Box]] if that is what you need.
    * @param bag the bag to which to add this ref each time the referent is created.
    */
  abstract class Ref[T >: Null] (bag :Bag) extends AutoCloseable {

    private[this] var _contents :T = null

    /** Creates (if necessary) and returns the referent. */
    def get :T = synchronized {
      if (_contents == null) {
        _contents = create()
        didCreate()
      }
      _contents
    }

    /** Nulls out this reference. Subsequent calls to [[get]] will cause the contents to be
      * recreated. */
    def close () {
      try willClose(_contents)
      finally _contents = null
    }

    /** Creates the referent. */
    protected def create () :T

    /** Called when our referent has been created. */
    protected def didCreate () {
      bag += this
    }

    /** Called just before we clear our referent. */
    protected def willClose (ref :T) {}
  }

  /** A ref that holds an `AutoCloseable` which is created on demand and closed and released when
    * the box itself is closed.
    */
  abstract class Box[C >: Null <: AutoCloseable] (bag :Bag) extends Ref[C](bag) {
    override protected def willClose (ref :C) {
      if (ref != null) ref.close()
    }
  }
}
