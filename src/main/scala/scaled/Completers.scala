//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import java.io.File

/** Some useful completion functions. */
object Completers {

  /** Returns a completer on buffer name.
    * @param except a set of buffer names to exclude from completion.
    */
  def buffer (editor :Editor, except :Set[String]) :String => Set[String] =
    (prefix :String) => Set() ++ editor.buffers.collect {
      case (buf) if (!except(buf.name) && (buf.name startsWith prefix)) => buf.name
    }

  /** Returns a completer on buffer name. */
  def buffer (editor :Editor) :String => Set[String] = buffer(editor, Set())

  /** A completer on file system files. */
  val file :String => Set[String] = path => path lastIndexOf File.separatorChar match {
    case -1  => expand(File.listRoots.head /*TODO*/, path)
    case idx => expand(new File(path.substring(0, idx)), path.substring(idx+1))
  }

  private def expand (dir :File, prefix :String) :Set[String] = {
    val file = new File(dir, prefix)
    if (file.exists) {
      if (file.isDirectory) Set() ++ file.listFiles map(format)
      else Set(format(file))
    } else Set() ++ dir.listFiles filter(_.getName startsWith prefix) map(format)
  }

  private def format (file :File) = {
    val path = file.getAbsolutePath
    if (file.isDirectory) path + File.separator else path
  }
}
