//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

/** Handles completions. */
abstract class Completer {

  /** Completes `prefix`, returning a set of completions.
    * The completions should be complete values, not suffixes to be applied to `prefix. */
  def apply (prefix :String) :Set[String]

  /** If the value being completed is a path, this separator will be used to omit the path elements
    * shared by the currently displayed prefix and the current completions. */
  def pathSeparator :Option[String] = None

  /** If true, the user will not be allowed to complete the selection with a string that is not a
    * valid completion. */
  def requireCompletion :Boolean = false
}

/** Some useful completion functions. */
object Completer {

  /** A noop completer. */
  val none :Completer = new Completer {
    def apply (prefix :String) = Set(prefix)
  }

  /** Returns a completer over `names`.
    * @param requireComp whether to require a completion from the supplied set. */
  def from (names :Set[String], requireComp :Boolean) :Completer = new Completer {
    def apply (prefix :String) = names filter (_ startsWith prefix)
    override def requireCompletion = requireComp
  }

  /** Returns a completer over `things` using `nameFn` to obtain  thing's name.
    * @param requireComp whether to require a completion from the supplied set. */
  def from[T] (things :Seq[T], requireComp :Boolean)(nameFn :T => String) :Completer =
    from(Set() ++ things map nameFn, requireComp)

  /** Returns a completer on buffer name. */
  def buffer (editor :Editor) :Completer = from(editor.buffers, false)(_.name)

  /** Returns a completer on buffer name.
    * @param except a set of buffer names to exclude from completion.
    */
  def buffer (editor :Editor, except :Set[String]) :Completer = new Completer {
    def apply (prefix :String) = Set() ++ editor.buffers.collect {
      case (buf) if (!except(buf.name) && (buf.name startsWith prefix)) => buf.name
    }
  }

  /** A completer on file system files. */
  val file :Completer = new Completer {
    def apply (path :String) = path lastIndexOf File.separatorChar match {
      case -1  => expand(File.listRoots.head /*TODO*/, path)
      case idx => expand(new File(path.substring(0, idx+1)), path.substring(idx+1))
    }
    override def pathSeparator = Some(File.separator)
  }

  private def expand (dir :File, prefix :String) :Set[String] = {
    val edir = massage(dir)
    val files = if (edir.exists) edir.listFiles else Array[File]()
    val matches = Set() ++ files filter(_.getName startsWith prefix) map(format)
    val file = new File(edir, prefix)
    if (!file.exists) matches
    else if (file.isDirectory && matches.size == 1) Set() ++ file.listFiles map(format)
    else matches
  }

  private def massage (dir :File) = {
    if (dir.getName == "~") new File(System.getProperty("user.home"))
    else dir // TODO: map // to root of file system?
  }

  private def format (file :File) = {
    val path = file.getAbsolutePath
    if (file.isDirectory) path + File.separator else path
  }
}
