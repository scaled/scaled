//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.Path
import java.util.HashMap
import scaled._

/** Handles the machinery underlying the [[Config]] configuration system. */
class ConfigManager (log :Logger, watchSvc :WatchService) {
  import Config._

  final val EditorName = "editor"

  /** Returns the editor config for the specified scope. */
  def editorConfig (scope :Scope) :Config = repo(scope).editorConfig

  /** Returns the [[Config]] instance for `mode`. */
  def resolveConfig (scope :Scope, mode :String, defs :List[Config.Defs]) :Config =
    repo(scope).modeConfig(mode, defs)

  private def repo (scope :Scope) :ConfigRepo = Mutable.getOrPut(
    _repos, scope, new ConfigRepo(scope, scope.next.map(repo)))

  private final val FileSuff = ".properties"
  private final val ModeSuff = "-mode"
  private final val _repos = new HashMap[Scope,ConfigRepo]()

  private class ConfigRepo (scope :Scope, parent :Option[ConfigRepo]) {
    private val _configDir = Filer.requireDir(scope.root.resolve("Config"))
    private val _modeConfigs = new HashMap[String,ConfigImpl]()
    private val _editor = loadConfig(EditorName, EditorConfig :: Nil, _.editorConfig)

    // listen for changes to files in our config directory and reload configs therefor; note: we
    // never unregister this watch so we don't need to keep the handle around
    watchSvc.watchDir(_configDir, new Watcher() {
      override def onCreate (dir :Path, name :String) = checkReload(name)
      override def onModify (dir :Path, name :String) = checkReload(name)
      protected def checkReload (name :String) {
        if (name endsWith FileSuff) {
          val root = name dropRight FileSuff.length
          if (root endsWith ModeSuff) {
            val mode = root dropRight ModeSuff.length
            Option(_modeConfigs.get(mode)) foreach { _.read(log) }
          } else if (root == EditorName) _editor.read(log)
        }
      }
    })

    def editorConfig :ConfigImpl = _editor
    def modeConfig (mode :String, defs :List[Config.Defs]) :ConfigImpl = Mutable.getOrPut(
      _modeConfigs, mode, loadConfig(s"$mode-mode", defs, _.modeConfig(mode, defs)))

    private def loadConfig (name :String, defs :List[Config.Defs],
                            parentFn :ConfigRepo => ConfigImpl) :ConfigImpl = {
      val file = _configDir.resolve(name + FileSuff)
      new ConfigImpl(name, file, scope, defs, parent.map(parentFn)).read(log)
    }
  }
}
