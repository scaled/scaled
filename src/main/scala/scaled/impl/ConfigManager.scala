//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import scala.collection.mutable.{Map => MMap}
import scaled._

/** Handles the machinery underlying the [[Config]] configuration system. */
class ConfigManager (app :Main) {

  final val GlobalName = "editor"

  /** The global editor config. */
  def globalConfig :Config = _global

  /** Returns the [[Config]] instance for `mode`. */
  def resolveConfig (mode :String, defs :List[Config.Defs]) :Config =
    _configs.getOrElseUpdate(mode, loadConfig(mode, defs))

  /** Returns the file that contians mode's configuration. */
  def configFile (mode :String) :File = new File(_configDir, mode + FileSuff)

  /** Returns the current configuration for `mode` as properties text, suitable for jamming into its
    * config file. */
  def configText (mode :String) :Option[Seq[String]] = _configs.get(mode).map(_.toProperties)

  private final val FileSuff = ".properties"
  private val _configDir = Filer.requireDir(new File(app.metaDir, "Config"))
  private val _configs = MMap[String,ConfigImpl]()
  private val _global = loadConfig(GlobalName, EditorConfig :: Nil)

  // listen for changes to files in our config directory and reload configs therefor; note: we
  // never unregister this watch so we don't need to keep the handle around
  app.watchMgr.watchDir(_configDir, new Watcher() {
    override def onCreate (dir :File, name :String) = checkReload(name)
    override def onModify (dir :File, name :String) = checkReload(name)
    protected def checkReload (name :String) {
      if (name endsWith FileSuff) {
        val mode = name dropRight FileSuff.length
        _configs.get(mode) foreach { cfg => readFileInto(mode, cfg) }
        if (mode == GlobalName) readFileInto(GlobalName, _global)
      }
    }
  })

  private def loadConfig (mode :String, defs :List[Config.Defs]) :ConfigImpl = {
    val parent = if (mode == GlobalName) None else Some(_global)
    readFileInto(mode, new ConfigImpl(mode, defs, parent))
  }

  private def readFileInto (mode :String, cfg: ConfigImpl) :ConfigImpl = {
    val file = configFile(mode)
    if (file.exists()) ConfigImpl.readInto(file, cfg)
    cfg
  }
}
