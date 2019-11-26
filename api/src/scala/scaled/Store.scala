//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.{File, FileNotFoundException, FileReader, FileWriter, Reader, Writer}
import java.io.{BufferedReader, BufferedWriter, InputStreamReader, StringReader}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.file.attribute.PosixFileAttributeView
import java.util.zip.ZipFile

/** A place from which to read and optionally to which to write data. This is slightly more general
  * than file in that it allows Scaled to seamlessly read data from `.zip`. and `.jar` file entries,
  * and could conceivably be extended to allow reading and writing of files over the network.
  */
abstract class Store extends Comparable[Store] {

  /** The name of this store. */
  def name :String

  /** Returns the path to the file represented by this store, if appropriate. Stores that do not
    * represent a file-system file will return `None`. */
  def file :Option[Path] = None

  /** The path to the parent of this store. If the path represents a directory, it will include a
    * trailing path separator. */
  def parent :String

  /** Returns true if this store already exists. */
  def exists :Boolean

  /** Returns true if this store cannot be written, false otherwise. */
  def readOnly :Boolean

  /** Passes the contents of this store as a `Reader` to `fn`. The `Reader` is automatically closed
    * when `fn` returns. */
  def read (fn :Reader => Unit) :Unit = {
    val r = reader
    try fn(r)
    finally r.close()
  }

  /** Writes `lines` to this store. */
  def write (lines :Iterable[Store.Writable]) :Unit =
    throw new UnsupportedOperationException(s"$name is not writable.")

  /** Returns last modified time of backing file, or 0L for non-file stores. */
  def lastModified :Long = file.map { path =>
    try if (Files.exists(path)) Files.getLastModifiedTime(path).toMillis else 0L
    catch {
      case e :Throwable => System.err.println(s"lastModified failed $path: $e") ; 0L
    }
  } || 0L

  override def compareTo (other :Store) :Int = parent.compareTo(other.parent) match {
    case 0 => name.compareTo(other.name)
    case n => n
  }

  // re-abstract these methods to be sure we don't forget to implement them
  override def equals (other :Any) :Boolean
  override def hashCode :Int

  /** Returns a `Reader` for this store's contents. */
  protected def reader :Reader
}

object Store {

  /** Used when writing things to stores. */
  trait Writable {
    def write (out :Writer) :Unit
  }

  /** Creates a store the file represented by `path`. If the path contains `!` it is assumed to be
    * an archive path of the form `path/to/foo.zip!path/to/entry`. */
  def apply (path :String) :Store = path.split("!", 2) match {
    case Array(zip, entry) => ZipEntryStore(Paths.get(zip), entry)
    case _                 => FileStore(Paths.get(path))
  }

  /** Creates a store for `path`. */
  def apply (path :Path) :Store = FileStore(path)

  /** Creates a scratch store with the supplied name and parent directory. */
  def scratch (name :String, pdir :Path = userDir) = new TextStore(parent(pdir), name, "")

  /** Creates a scratch store with the supplied name and which inherits its working directory
    * from `inherit`. This is often appropriate when opening a scratch buffer while viewing an
    * existing buffer. */
  def scratch (name :String, inherit :Store) = new TextStore(inherit.parent, name, "")

  /** Creates a read-only text store with the supplied `name` and `text`. */
  def text (name :String, text :String, pdir :Path = userDir) =
    new TextStore(parent(pdir), name, text)

  /** Returns the real path of `path` iff it exists. Otherwise returns `path` as is. */
  def realPath (path :Path) = if (Files.exists(path)) path.toRealPath() else path

  /** Used to read lines via [readLines]. */
  abstract class LineReader {
    /** Applies this reader to a single line.
      * @param data an arbitrary buffer of characters.
      * @param start the offset of the start of the line in `data`.
      * @param end the offset of the end of the line in `data`.
      * @param fileOffset othe offset of the start of the line in the entire file (not just the
      * `data` buffer).
      */
    def apply (data :Array[Char], start :Int, end :Int, fileOffset :Int) :Unit
  }

  /** Returns a line by line reader for `r` that processes lines with `lr`. */
  def reader (lr :LineReader) = (r :Reader) => {
    val buffer = new Array[Char](32768)
    var storeOffset = 0
    var read = 0
    var leftover = 0
    while (read >= 0) {
      read = r.read(buffer, leftover, buffer.length-leftover)
      if (read > 0) {
        val have = leftover + read
        var start = 0
        var next = start
        while (next < have) {
          val c = buffer(next)
          // TEMP: hackery to remove tabs; TODO: remove when we support tabs
          if (c == '\t') buffer(next) = ' '
          // TODO: handle bare \r?
          if (c == '\r' || c == '\n') {
            lr(buffer, start, next, storeOffset)
            next += 1
            if (next < have-1) {
              val nc = buffer(next)
              if (c == '\r' && nc == '\n') next += 1
            }
            storeOffset += (next-start)
            start = next
          } else {
            next += 1
          }
        }
        leftover = have-start
        if (leftover > 0) System.arraycopy(buffer, start, buffer, 0, leftover)
      }
    }
    if (leftover > 0) lr(buffer, 0, leftover, storeOffset)
  }

  private def userDir = Paths.get(System.getProperty("user.dir"))
  private def parent (pdir :Path) = pdir.toString + File.separator
}

/** A store that represents a normal file. */
class FileStore private (val path :Path) extends Store {

  override def name = path.getFileName.toString
  override def file = Some(path)
  override def parent = path.getParent.toString+File.separator
  override def exists = Files.exists(path)
  override def readOnly = exists && !Files.isWritable(path)

  override def reader = if (exists) new FileReader(path.toFile) else new StringReader("")

  override def write (lines :Iterable[Store.Writable]) :Unit = {
    Files.createDirectories(path.getParent) // make sure our parent directory exists
    // TODO: file encoding?
    val temp = path.resolveSibling(name + "~")
    val canPosix = Files.getFileAttributeView(path, classOf[PosixFileAttributeView]) != null
    val perms = if (exists && canPosix) Files.getPosixFilePermissions(path) else null
    val out = new BufferedWriter(new FileWriter(temp.toFile))
    try {
      val iter = lines.iterator ; while (iter.hasNext) {
        iter.next.write(out)
        if (iter.hasNext) out.newLine()
      }
      out.close()
      Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING)
      if (perms != null && !perms.isEmpty) Files.setPosixFilePermissions(path, perms)

    } finally {
      Files.deleteIfExists(temp) // if something goes wrong, delete our temp file
    }
  }

  override def equals (other :Any) = other match {
    case os :FileStore => path == os.path
    case _ => false
  }
  override def hashCode = path.hashCode
  override def toString = path.toString
}

object FileStore {
  def apply (path :Path) = new FileStore(Store.realPath(path))
  def unapply (store :FileStore) :Option[Path] = Some(store.path)
}

/** A store that represents an entry in a zip file.
  * @param zipFile the zip file from which we read `entry`.
  * @param entry the zip file entry that backs this store.
  */
class ZipEntryStore private (val zipFile :Path, val entry :String) extends Store {
  lazy private val zfile = new ZipFile(zipFile.toFile)
  lazy private val zentry = zfile.getEntry(entry)

  override def name = Paths.get(entry).getFileName.toString
  override def parent = zipFile.toString
  override def exists = Files.exists(zipFile) && zentry != null
  override def readOnly = true
  override def reader = if (exists) zentry match {
    case null  => new StringReader("")
    case entry => new InputStreamReader(zfile.getInputStream(entry), "UTF-8")
  } else new StringReader("")

  override def equals (other :Any) = other match {
    case os :ZipEntryStore => zipFile == os.zipFile && entry == os.entry
    case _ => false
  }
  override def hashCode = zipFile.hashCode ^ entry.hashCode
  override def toString = zipFile.toString + "!" + entry
}

object ZipEntryStore {
  def apply (zipFile :Path, entry :String) = new ZipEntryStore(Store.realPath(zipFile), entry)
  def unapply (store :ZipEntryStore) :Option[(Path,String)] = Some((store.zipFile, store.entry))
}

class TextStore (val parent :String, val name :String, val text :String) extends Store {

  override def exists = false
  override def readOnly = true
  override def reader = new StringReader(text)

  override def equals (other :Any) = other match {
    case os :TextStore => name == os.name && parent == os.parent && text == os.text
    case _ => false
  }
  override def hashCode = name.hashCode ^ parent.hashCode
  override def toString = s"$name @ $parent"
}
