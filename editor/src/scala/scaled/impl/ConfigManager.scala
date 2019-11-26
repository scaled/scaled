//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.Path
import java.util.HashMap
import scaled._

/** Handles the machinery underlying the [[Config]] configuration system. */
class ConfigManager (editor :Editor, log :Logger, watchSvc :WatchService)
    extends AbstractService with ConfigService {
  import Config._

  final val EditorName = "editor"
  private def globalScope = editor.config.scope

  /** Returns the editor config for the specified scope. */
  def editorConfig (scope :Scope) :Config = repo(scope).editorConfig

  override def resolveModeConfig (scope :Scope, name :String, defs :List[Defs]) =
    repo(scope).modeConfig(name, defs)

  override def serviceConfigs = repo(globalScope).serviceConfigs
  override def resolveServiceConfig (name :String, defs :List[Defs]) =
    repo(globalScope).serviceConfig(name, defs)

  // unused; TODO: ?
  override def didStartup () :Unit = {}
  override def willShutdown () :Unit = {}

  private def repo (scope :Scope) :ConfigRepo = Mutable.getOrPut(
    _repos, scope, new ConfigRepo(scope, scope.next.map(repo)))

  private final val FileSuff = ".properties"
  private final val ModeSuff = "-mode"
  private final val _repos = new HashMap[Scope,ConfigRepo]()

  private class ConfigRepo (scope :Scope, parent :Option[ConfigRepo]) {
    private val _configDir = Filer.requireDir(scope.root.resolve("Config"))
    private val _modeConfigs = new HashMap[String,ConfigImpl]()
    private val _serviceConfigs = new HashMap[String,ConfigImpl]()
    private val _editor = loadConfig(EditorName, EditorConfig :: Nil, _.editorConfig)

    // listen for changes to files in our config directory and reload configs therefor; note: we
    // never unregister this watch so we don't need to keep the handle around
    watchSvc.watchDir(_configDir, new Watcher() {
      override def onCreate (dir :Path, name :String) = checkReload(name)
      override def onModify (dir :Path, name :String) = checkReload(name)
      protected def checkReload (name :String) :Unit = {
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
    def modeConfig (name :String, defs :List[Config.Defs]) :ConfigImpl = Mutable.getOrPut(
      _modeConfigs, name, loadConfig(s"$name-mode", defs, _.modeConfig(name, defs)))
    def serviceConfigs = _serviceConfigs.toMapV.toSeq
    def serviceConfig (name :String, defs :List[Config.Defs]) :ConfigImpl = Mutable.getOrPut(
      _serviceConfigs, name, loadConfig(s"$name-service", defs, _.serviceConfig(name, defs)))

    private def loadConfig (name :String, defs :List[Config.Defs],
                            parentFn :ConfigRepo => ConfigImpl) :ConfigImpl = {
      val file = _configDir.resolve(name + FileSuff)
      new ConfigImpl(name, file, scope, defs, parent.map(parentFn)).read(log)
    }
  }
}
