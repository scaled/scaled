//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.io.{File, FileNotFoundException, FileReader, FileWriter, Reader, Writer}
import java.io.{BufferedReader, BufferedWriter, InputStreamReader, StringReader}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.zip.ZipFile

/** A place from which to read and optionally to which to write data. This is slightly more general
  * than file in that it allows Scaled to seamlessly read data from `.zip`. and `.jar` file entries,
  * and could conceivably be extended to allow reading and writing of files over the network.
  */
abstract class Store {

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

  /** Reads the contents of this store line by line, applying `fn` to each line in the store. `fn`
    * is also passed the character offset of the start of the line. */
  def readLines (fn :(String, Int) => Unit) {
    read({ r =>
      val buffed = new BufferedReader(r)
      var offset = 0
      var line :String = buffed.readLine()
      while (line != null) {
        fn(line, offset)
        offset += line.length + 1 // TODO: handle \r\n?
        line = buffed.readLine()
      }
    })
  }

  /** Passes the contents of this store as a `Reader` to `fn`. The `Reader` is automatically closed
    * when `fn` returns. */
  def read (fn :Reader => Unit) {
    val r = reader
    try fn(r)
    finally r.close()
  }

  /** Writes `lines` to this store. */
  def write (lines :Iterable[Store.Writable]) :Unit =
    throw new UnsupportedOperationException(s"$name is not writable.")

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

  def realPath (path :Path) = if (Files.exists(path)) path.toRealPath() else path
}

/** A store that represents a normal file. */
class FileStore private (val path :Path) extends Store {

  override def name = path.getFileName.toString
  override def file = Some(path)
  override def parent = path.getParent.toString+File.separator
  override def exists = Files.exists(path)
  override def readOnly = exists && !Files.isWritable(path)

  override def reader = if (exists) new FileReader(path.toFile) else new StringReader("")

  override def write (lines :Iterable[Store.Writable]) {
    Files.createDirectories(path.getParent) // make sure our parent directory exists
    // TODO: file encoding?
    val temp = path.resolveSibling(name + "~")
    val perms = if (exists) Files.getPosixFilePermissions(path) else null
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
  override def exists = zentry != null
  override def readOnly = true
  override def reader = zentry match {
    case null  => new StringReader("")
    case entry => new InputStreamReader(zfile.getInputStream(entry), "UTF-8")
  }

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

class TextStore (val name :String, val parent :String, val text :String) extends Store {

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
