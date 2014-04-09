//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

/** Some useful completion functions. */
object Completers {
  type Completer = String => Set[String]

  /** A noop completer. */
  val none :Completer = (prefix :String) => Set(prefix)

  /** Returns a completer over `names`. */
  def from (names :Set[String]) :Completer =
    (prefix :String) => names filter (_ startsWith prefix)

  /** Returns a completer over `things` using `nameFn` to obtain  thing's name. */
  def from[T] (things :Seq[T])(nameFn :T => String) :Completer = from(Set() ++ things map nameFn)

  /** Returns a completer on buffer name. */
  def buffer (editor :Editor) :Completer = from(editor.buffers)(_.name)

  /** Returns a completer on buffer name.
    * @param except a set of buffer names to exclude from completion.
    */
  def buffer (editor :Editor, except :Set[String]) :Completer =
    (prefix :String) => Set() ++ editor.buffers.collect {
      case (buf) if (!except(buf.name) && (buf.name startsWith prefix)) => buf.name
    }

  /** A completer on file system files. */
  val file :Completer = path => path lastIndexOf File.separatorChar match {
    case -1  => expand(File.listRoots.head /*TODO*/, path)
    case idx => expand(new File(path.substring(0, idx+1)), path.substring(idx+1))
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
