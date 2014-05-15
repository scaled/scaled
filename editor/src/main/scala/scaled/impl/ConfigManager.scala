//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.Properties

/** Handles the machinery underlying the [[Config]] configuration system. */
class ConfigManager (log :Logger, metaSvc :MetaService, watchSvc :WatchService) {

  final val EditorName = "editor"

  /** The global editor config. */
  def editorConfig :Config = _editor

  /** Returns the [[Config]] instance for `mode`. */
  def resolveConfig (mode :String, defs :List[Config.Defs]) :Config =
    _configs.getOrElseUpdate(mode, loadConfig(mode, defs))

  /** Returns the file that contians mode's configuration. */
  def configFile (mode :String) :File = new File(_configDir, mode + FileSuff)

  /** Returns the current configuration for `mode` as properties text, suitable for jamming into its
    * config file. */
  def configText (mode :String) :Option[Seq[String]] =
    if (mode == EditorName) Some(_editor.toProperties)
    else _configs.get(mode).map(_.toProperties)

  private final val FileSuff = ".properties"
  private val _configDir = Filer.requireDir(metaSvc.metaFile("Config"))
  private val _configs = MMap[String,ConfigImpl]()
  private val _editor = loadConfig(EditorName, EditorConfig :: Nil)

  // listen for changes to files in our config directory and reload configs therefor; note: we
  // never unregister this watch so we don't need to keep the handle around
  watchSvc.watchDir(_configDir, new Watcher() {
    override def onCreate (dir :File, name :String) = checkReload(name)
    override def onModify (dir :File, name :String) = checkReload(name)
    protected def checkReload (name :String) {
      if (name endsWith FileSuff) {
        val mode = name dropRight FileSuff.length
        _configs.get(mode) foreach { cfg => readFileInto(mode, cfg) }
        if (mode == EditorName) readFileInto(EditorName, _editor)
      }
    }
  })

  private def loadConfig (mode :String, defs :List[Config.Defs]) :ConfigImpl = {
    val parent = if (mode == EditorName) None else Some(_editor)
    readFileInto(mode, new ConfigImpl(mode, defs, parent))
  }

  private def readFileInto (mode :String, cfg: ConfigImpl) :ConfigImpl = {
    val file = configFile(mode)
    if (file.exists()) Properties.read(log, file)(cfg.init(log))
    cfg
  }
}
