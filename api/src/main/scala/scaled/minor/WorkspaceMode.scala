//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._

object WorkspaceConfig extends Config.Defs {
  import EditorConfig._

  /** The history ring for workspace names. */
  val nameHistory = fnKey(cfg => new Ring(cfg(historySize)))
}

@Minor(name="workspace", tags=Array("*"),
       desc="""A minor mode that provides workspace-related functionality.""")
class WorkspaceMode (env :Env) extends MinorMode(env) {
  import WorkspaceConfig._

  override def configDefs = WorkspaceConfig :: super.configDefs
  // override def keymap = super.keymap. // TODO

  @Fn("Creates a new workspace.")
  def createWorkspace () {
    editor.mini.read(s"Name:", "", config(nameHistory), Completer.none) onSuccess(wsvc.create)
  }

  @Fn("Opens an existing workspace.")
  def openWorkspace () {
    val comp = Completer.from(wsvc.list, true)
    editor.mini.read(s"Name:", "", config(nameHistory), comp) onSuccess(wsvc.open)
  }

  private val wsvc = env.msvc.service[WorkspaceService]
}
