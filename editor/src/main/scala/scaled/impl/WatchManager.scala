//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.nio.file.SensitivityWatchEventModifier
import java.nio.file.{FileSystems, Path, WatchKey, WatchEvent}
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scaled._

/** Handles watching the filesystem for changes. */
class WatchManager (log :Logger) extends AbstractService with WatchService {
  import java.nio.file.StandardWatchEventKinds._

  private val _service = FileSystems.getDefault.newWatchService()
  private val _watches = new ConcurrentHashMap[WatchKey,WatchImpl]()
  private val _watcher = new Thread("WatchManager") {
    override def run () = { while (true) pollWatches() }
  }
  _watcher.setDaemon(true)
  _watcher.start()

  private case class WatchImpl (key :WatchKey, dir :Path, watcher :Watcher) extends Watch {
    def dispatch (ev :WatchEvent[_]) = {
      val kind = ev.kind ; val name = ev.context.toString
      onMainThread(kind match {
        case ENTRY_CREATE => watcher.onCreate(dir, name)
        case ENTRY_DELETE => watcher.onDelete(dir, name)
        case ENTRY_MODIFY => watcher.onModify(dir, name)
        case _ => log.log(s"Unknown event type [dir=$dir, kind=$kind, ctx=$name]")
      })
    }
    def close () {
      key.cancel()
      log.log(s"Cleared watch: $dir")
      _watches.remove(key)
    }
  }

  // TODO...
  override def didStartup () {}
  override def willShutdown () {}

  /** Registers a watch on `file`. `watcher` will be invoked (on the main JavaFX thread) when `file`
    * is modified or deleted. */
  def watchFile (file :Path, watcher :Path => Unit) :Watch = {
    val name = file.getFileName.toString
    watchDir(file.getParent, new Watcher() {
      override def onCreate (dir :Path, child :String) = if (child == name) watcher(file)
      override def onModify (dir :Path, child :String) = if (child == name) watcher(file)
      override def onDelete (dir :Path, child :String) = if (child == name) watcher(file)
    })
  }

  /** Registers a watch on `dir`. `watcher` will be invoked (on the main JavaFX thread) when any
    * files are created, modified or deleted in `dir`. */
  def watchDir (dir :Path, watcher :Watcher) :Watch = {
    val kinds = Array(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY).
      asInstanceOf[Array[WatchEvent.Kind[_]]] // oh Scala, you devil
    val key = dir.register(_service, kinds, SensitivityWatchEventModifier.HIGH)
    val impl = WatchImpl(key, dir, watcher)
    log.log(s"Created watch: $dir")
    _watches.put(key, impl)
    impl
  }

  private def pollWatches () :Unit = try {
    import scala.collection.convert.WrapAsScala._
    // wait for key to be signalled
    val key = _service.take()
    val watcher = _watches.get(key)
    if (watcher != null) {
      key.pollEvents() foreach watcher.dispatch
      // if we can't reset the key (the dir went away or something), clear it out
      if (!key.reset()) watcher.close()
    }
  } catch {
    case ie :InterruptedException => // loop!
    case ex :Exception => log.log("pollWatches failure", ex)
  }
}
