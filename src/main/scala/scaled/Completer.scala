//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File
import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

/** Handles completions. */
abstract class Completer[T] {

  /** Completes `prefix`, returning a set of completions.
    * The completions should be complete values, not suffixes to be applied to `prefix. */
  def apply (prefix :String) :SortedMap[String,T]

  /** Requests to commit this completion with the specified current value. If `Some(r)` is
    * returned, the completion will finish with that result. If `None` is returned, the
    * user will be required to keep going until they provide a valid completion.
    *
    * The default implementation checks whether current is currently a valid completion, and
    * uses it, if so. Otherwise it calls `fromString` and uses that result if possible. Finally
    * it attempts to return the lexically first current completion (which is what will be
    * displayed at the top of the completions list).
    */
  def commit (current :String) :Option[T] = {
    val comps = apply(current)
    comps.get(current) orElse fromString(current) orElse comps.headOption.map(_._2)
  }

  /** If the value being completed is a path, this separator will be used to omit the path elements
    * shared by the currently displayed prefix and the current completions. */
  def pathSeparator :Option[String] = None

  /** Gives the completer a chance to "massage" the currently displayed prefix. By default the
    * prefix is replaced with the longest shared prefix of all the completions. In normal
    * circumstnaces this is useful, but if a completer is doing special stuff, it might not be.
    */
  def massageCurrent (cur :String, comps :Iterable[String]) :String =
    comps reduce sharedPrefix

  /** Generates a `T` from the supplied string. Return `None` to restrict the user to selecting
    * from one of the returned prefixes, return `Some(t)` to allow the user to enter any string
    * and have it turned into a valid result. */
  protected def fromString (value :String) :Option[T] = None

  /** Converts `rs` into a map, obtaining the key via `fn`. If multiple entries map to the same name
    * via `fn`, an arbitrary selection is made. */
  protected def mapBy (rs :Seq[T], fn :(T => String)) :SortedMap[String,T] =
    TreeMap(rs.map(r => (fn(r), r)) :_*)

  /** Returns the longest shared prefix of `a` and `b`. Matches case loosely, using uppercase
    * only when both strings have the character in uppercase, lowercase otherwise. */
  protected def sharedPrefix (a :String, b :String) = if (b startsWith a) a else {
    val buf = new StringBuilder
    @inline @tailrec def loop (ii :Int) {
      if (ii < a.length && ii < b.length) {
        val ra = a.charAt(ii) ; val la = Character.toLowerCase(ra)
        val rb = b.charAt(ii) ; val lb = Character.toLowerCase(rb)
        if (la == lb) {
          // if everyone uses uppercase here, keep the prefix uppercase, otherwise lower
          val c = if (Character.isUpperCase(ra) && Character.isUpperCase(rb)) ra else la
          buf.append(c)
          loop(ii+1)
        }
      }
    }
    loop(0)
    buf.toString
  }
}

/** Some useful completion functions. */
object Completer {

  /** A noop completer for strings. */
  val none :Completer[String] = new Completer[String] {
    def apply (prefix :String) = TreeMap(prefix -> prefix)
    override protected def fromString (value :String) = Some(value)
  }

  /** Returns a completer over `names`.
    * @param requireComp whether to require a completion from the supplied set. */
  def from (names :Set[String], requireComp :Boolean) :Completer[String] = new Completer[String] {
    def apply (prefix :String) = selfMap(names.filter(_ startsWith prefix).toSeq)
    override protected def fromString (value :String) = if (requireComp) None else Some(value)
  }

  /** Returns a completer over `things` using `nameFn` to obtain  thing's name.
    * @param requireComp whether to require a completion from the supplied set. */
  def from[T] (things :Seq[T], requireComp :Boolean)(nameFn :T => String) :Completer[String] =
    from(Set() ++ things map nameFn, requireComp)

  /** Returns a completer on buffer name. */
  def buffer (editor :Editor) :Completer[String] = from(editor.buffers, false)(_.name)

  /** Returns a completer on buffer name.
    * @param except a set of buffer names to exclude from completion.
    */
  def buffer (editor :Editor, except :Set[String]) :Completer[String] = new Completer[String] {
    def apply (prefix :String) = selfMap(editor.buffers.collect {
      case (buf) if (!except(buf.name) && (buf.name startsWith prefix)) => buf.name
    })
    override protected def fromString (value :String) = Some(value)
  }

  /** Replaces newlines with whitespace. This should be called on any string that will be used
    * as a completion key which may potentially contain newlines. File names are the primary
    * culprit here. */
  def defang (name :String) = name.replace('\n', ' ').replace('\r', ' ')

  private def selfMap (names :Seq[String]) = TreeMap(names.map(n => (n, n)) :_*)

  /** A completer on file system files. */
  val file :Completer[File] = new Completer[File] {
    def apply (path :String) = path lastIndexOf File.separatorChar match {
      case -1  => expand(File.listRoots.head /*TODO*/, path)
      case idx => expand(new File(path.substring(0, idx+1)), path.substring(idx+1))
    }
    override def pathSeparator = Some(File.separator)
    override protected def fromString (value :String) = Some(new File(value))

    private def expand (dir :File, prefix :String) = {
      val edir = massage(dir)
      val files = if (edir.exists) edir.listFiles else Array[File]()
      val matches = files.filter(f => defang(f.getName) startsWith prefix)
      val file = new File(edir, prefix)
      if (file.exists && file.isDirectory && matches.length > 1) mapBy(file.listFiles, format)
      else mapBy(matches, format)
    }

    private def massage (dir :File) = {
      if (dir.getName == "~") new File(System.getProperty("user.home"))
      else dir // TODO: map // to root of file system?
    }

    private def format (file :File) = {
      val path = defang(file.getAbsolutePath)
      if (file.isDirectory) path + File.separator else path
    }
  }
}
