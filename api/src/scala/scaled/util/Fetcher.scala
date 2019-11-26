//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import java.net.URL
import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{Files, Path, StandardOpenOption}
import scaled._

/**
 * Helper functions for downloading things via HTTP and unpacking them if needed.
 */
object Fetcher {

  /** Used to report `fetch` progress. */
  trait Progress {
    /** Indicates that the process is `pct` percent complete. */
    def progress (pct :Int) :Unit
  }

  /** A `Progress` instance that ignores progress reports. */
  val Ignore = new Progress() {
    def progress (pct :Int) :Unit = {}
  }

  /**
   * Downloads `url` into `target` using a background task provided by `exec`.
   * Download progress is reported to `obs`.
   * @return a future that completes with `target` on success, or an exception on failure.
   */
  def fetch (exec :Executor, url :URL, target :Path,
             obs :Progress = Ignore) :Future[Path] = exec.runAsync {
    val conn = url.openConnection()
    using(Channels.newChannel(conn.getInputStream())) { inchan =>
      import StandardOpenOption._
      using(FileChannel.open(target, CREATE, WRITE, TRUNCATE_EXISTING)) { outchan =>
        val blockSize = 64*1024
        var read = 0L ; var total = 0L ; var pct = 0
        do {
          // we're relying on the fact that the source channel is blocking and thus transferFrom
          // shoud return > 0 until we reach end of stream; I'm not 100% sure this is the case but
          // the documentation and lazyweb have utterly failed be in giving guidance on how to
          // properly fully transfer the contents of an HTTP request
          read = outchan.transferFrom(inchan, total, blockSize)
          total += read
          val length = conn.getContentLength()
          val npct = if (length > 0) (100 * total / length).toInt else 0
          if (npct > pct) { pct = npct ; exec.runOnUI { obs.progress(npct) }}
        } while (read > 0)
        target
      }
    }
  }
}
