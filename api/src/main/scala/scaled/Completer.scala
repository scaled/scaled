//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/** Represents a computed completion. */
class Completion[T] (values :Iterable[T], format :T => String, sort :Boolean) {

  private[this] val (_comps, _map) = {
    val b = Map.newBuilder[String,T]
    val c = if (sort) TreeSet.newBuilder[String] else Seq.newBuilder[String]
    val viter = values.iterator
    while (viter.hasNext) {
      val value = viter.next
      val comp = format(value)
      b += (comp -> value)
      c += comp
    }
    (c.result, b.result)
  }

  /** Returns the completion display strings, in the order they should be displayed. */
  def comps :Iterable[String] = _comps

  /** Returns `Some` value associated with the completion `comp`, or `None`. */
  def apply (comp :String) :Option[T] = _map.get(comp)

  /** Returns the default value to use when committed with a non-matching value. */
  def defval :Option[T] = comps.headOption.map(_map)

  /** Gives the completion a chance to "massage" the currently displayed prefix. By default the
    * prefix is replaced with the longest shared prefix of all the completions. In normal
    * circumstnaces this is useful, but if a completer is doing special stuff, it might not be.
    */
  def massageCurrent (cur :String) :String = comps reduce sharedPrefix

  override def toString = _map.toString

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

/** Handles completions. */
abstract class Completer[T] {

  /** Completes `prefix`, returning a list of completions.
    * The completions should be complete values, not suffixes to be applied to `prefix. */
  def complete (prefix :String) :Completion[T]

  /** Requests to commit `comp` with the specified `current` value. If `Some(r)` is
    * returned, the completion will finish with that result. If `None` is returned, the
    * user will be required to keep going until they provide a valid completion.
    *
    * The default implementation checks whether current is currently a valid completion, and
    * uses it, if so. Otherwise it calls `fromString` and uses that result if possible. Finally
    * it attempts to return the lexically first current completion (which is what will be
    * displayed at the top of the completions list).
    */
  def commit (comp :Option[Completion[T]], current :String) :Option[T] = {
    comp.flatMap(_.apply(current)) orElse fromString(current) orElse comp.flatMap(_.defval)
  }

  /** If the value being completed is a path, this separator will be used to omit the path elements
    * shared by the currently displayed prefix and the current completions. */
  def pathSeparator :Option[String] = None

  /** Generates a `T` from the supplied string. Return `None` to restrict the user to selecting
    * from one of the returned prefixes, return `Some(t)` to allow the user to enter any string
    * and have it turned into a valid result. */
  protected def fromString (value :String) :Option[T] = None

  /** Returns a completion over `values` in iteration order. */
  protected def completion[T] (values :Iterable[T], format :T => String) =
    new Completion[T](values, format, false)

  /** Returns a completion over `values` in lexical order (of the formatted completions). */
  protected def sortedCompletion[T] (values :Iterable[T], format :T => String) =
    new Completion[T](values, format, true)

  /** Returns a sorted completion over some strings. */
  protected def stringCompletion (ss :Iterable[String]) = sortedCompletion(ss, identity[String])

  /** Returns true if `full` starts with `prefix`, ignoring case, false otherwise. */
  protected def startsWithI (prefix :String)(full :String) :Boolean = {
    @inline @tailrec def loop (ii :Int) :Boolean = {
      if (ii == prefix.length) true
      else {
        val ra = full.charAt(ii) ; val rb = prefix.charAt(ii)
        if (ra == rb) loop(ii+1)
        else {
          val la = Character.toLowerCase(ra) ; val lb = Character.toLowerCase(rb)
          if (la == lb) loop(ii+1)
          else false
        }
      }
    }
    if (prefix.length > full.length) false else loop(0)
  }
}

/** Some useful completion functions. */
object Completer {

  /** A noop completer for strings. */
  val none :Completer[String] = new Completer[String] {
    def complete (prefix :String) = stringCompletion(Seq(prefix))
    override protected def fromString (value :String) = Some(value)
  }

  /** Returns a completer over `names`.
    * @param requireComp whether to require a completion from the supplied set. */
  def from (names :Iterable[String], requireComp :Boolean) = new Completer[String] {
    def complete (prefix :String) = stringCompletion(names.filter(startsWithI(prefix)))
    override protected def fromString (value :String) = if (requireComp) None else Some(value)
  }

  /** Returns a completer over `things` using `nameFn` to obtain each thing's name. */
  def from[T] (things :Iterable[T])(nameFn :T => String) = new Completer[T]() {
    def complete (prefix :String) = completion(
      things.filter(t => startsWithI(prefix)(nameFn(t))), nameFn)
  }

  /** Returns a completer on buffer name. */
  def buffer (editor :Editor) :Completer[String] = from(editor.buffers.map(_.name), false)

  /** Returns a completer on buffer name.
    * @param except a set of buffer names to exclude from completion.
    */
  def buffer (editor :Editor, except :Set[String]) :Completer[String] =
    from(editor.buffers collect { case (buf) if (!except(buf.name)) => buf.name }, false)

  /** Replaces newlines with whitespace. This should be called on any string that will be used
    * as a completion key which may potentially contain newlines. File names are the primary
    * culprit here. */
  def defang (name :String) = name.replace('\n', ' ').replace('\r', ' ')

  /** A completer on file system files. */
  val file :Completer[File] = new Completer[File] {
    def complete (path :String) = path lastIndexOf File.separatorChar match {
      case -1  => expand(File.listRoots.head /*TODO*/, path)
      case idx => expand(new File(path.substring(0, idx+1)), path.substring(idx+1))
    }
    override def pathSeparator = Some(File.separator)
    override protected def fromString (value :String) = Some(new File(value))

    private def ignore (file :File) :Boolean = file.getName.startsWith(".")

    private def expand (dir :File, prefix :String) = {
      val edir = massage(dir)
      val files = if (edir.exists) edir.listFiles.filterNot(ignore) else Array[File]()
      val matches = files.filter(f => startsWithI(prefix)(defang(f.getName)))
      val file = new File(edir, prefix)
      sortedCompletion(if (file.exists && file.isDirectory && matches.length > 1) file.listFiles
                       else matches, formatFile)
    }

    private def massage (dir :File) = {
      if (dir.getName == "~") new File(System.getProperty("user.home"))
      else dir // TODO: map // to root of file system?
    }

    private val formatFile = (file :File) => {
      val path = defang(file.getAbsolutePath)
      if (file.isDirectory) path + File.separator else path
    }
  }
}
