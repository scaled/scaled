//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.nio.file.SensitivityWatchEventModifier
import java.nio.file.{FileSystems, Path, WatchKey, WatchEvent}
import java.util.concurrent.ConcurrentHashMap
import scaled._

/** Handles watching the filesystem for changes. */
class WatchManager (log :Logger, exec :Executor) extends AbstractService with WatchService {
  import java.nio.file.StandardWatchEventKinds._

  // TODO...
  override def didStartup () {}
  override def willShutdown () {}

  override def watchFile (file :Path, watcher :Path => Unit) = {
    val name = file.getFileName.toString
    addWatch(file.getParent) { ev =>
      val evname = ev.context.toString
      if (evname == name) watcher(file)
    }
  }

  override def watchDir (dir :Path, watcher :Watcher) = addWatch(dir) { ev =>
    val kind = ev.kind ; val name = ev.context.toString
    kind match {
      case ENTRY_CREATE => watcher.onCreate(dir, name)
      case ENTRY_DELETE => watcher.onDelete(dir, name)
      case ENTRY_MODIFY => watcher.onModify(dir, name)
      case _ => log.log(s"Unknown event type [dir=$dir, kind=$kind, ctx=$name]")
    }
  }

  private def addWatch (dir :Path)(cb :JConsumer[WatchEvent[_]]) = byDir.get(dir).signal onValue cb

  private def pollWatches () :Unit = try {
    // wait for key to be signalled
    val key = service.take()
    val info = byKey.get(key)
    if (info != null) {
      key.pollEvents() foreach info.dispatch
      // if we can't reset the key (the dir went away or something), clear it out
      if (!key.reset()) info.close()
    }
  } catch {
    case ie :InterruptedException => // loop!
    case ex :Exception => log.log("pollWatches failure", ex)
  }

  private val kinds = Array(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY).
    asInstanceOf[Array[WatchEvent.Kind[_]]] // oh Scala, you devil

  private case class WatchInfo (dir :Path) {
    val signal = Signal[WatchEvent[_]](exec.ui)
    val key = dir.register(service, kinds, SensitivityWatchEventModifier.HIGH)
    byKey.put(key, this)
    log.log(s"Created watch: $dir")

    def dispatch (ev :WatchEvent[_]) {
      signal.emit(ev)
      if (!signal.hasConnections) close()
    }
    def close () {
      key.cancel()
      log.log(s"Cleared watch: $dir")
      byKey.remove(key)
      byDir.invalidate(dir)
    }
  }

  private val service = FileSystems.getDefault.newWatchService()
  private val byDir = Mutable.cacheMap[Path,WatchInfo](WatchInfo(_))
  private val byKey = new ConcurrentHashMap[WatchKey,WatchInfo]()
  private val watcher = new Thread("WatchManager") {
    override def run () = { while (true) pollWatches() }
  }
  watcher.setDaemon(true)
  watcher.start()
}
