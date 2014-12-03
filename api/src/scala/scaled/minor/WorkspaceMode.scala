//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._

@Minor(name="workspace", tags=Array("*"),
       desc="""A minor mode that provides workspace-related functionality.""")
class WorkspaceMode (env :Env) extends MinorMode(env) {

  @Fn("Creates a new workspace.")
  def createWorkspace () {
    window.mini.read(s"Name:", "", nameHistory, Completer.none) onSuccess(wsvc.create)
  }

  @Fn("Opens an existing workspace.")
  def openWorkspace () {
    val comp = Completer.from(wsvc.list)
    window.mini.read(s"Name:", "", nameHistory, comp) onSuccess(wsvc.open)
  }

  @Fn("Opens a new window in the current workspace.")
  def openWindow () {
    wspace.openWindow(None)
  }

  /** The history ring for workspace names. */
  protected def nameHistory = Workspace.historyRing(wspace, "workspace-name")

  private val wsvc = env.msvc.service[WorkspaceService]
}
