//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.minor

import scaled._
import scaled.util.BufferBuilder

@Minor(name="workspace", tags=Array("*"),
       desc="""A minor mode that provides workspace-related functionality.""")
class WorkspaceMode (env :Env) extends MinorMode(env) {

  override def keymap = super.keymap.
    bind("describe-workspace", "C-h w");

  @Fn("Creates a new workspace.")
  def createWorkspace () :Unit = {
    window.mini.read(s"Name:", "", nameHistory, Completer.none) onSuccess(wsvc.create)
  }

  @Fn("Opens an existing workspace.")
  def openWorkspace () :Unit = {
    val comp = Completer.from(wsvc.list)
    window.mini.read(s"Name:", "", nameHistory, comp) onSuccess(wsvc.open)
  }

  @Fn("Opens a new window in the current workspace.")
  def openWindow () :Unit = {
    wspace.openWindow(None).focus.visit(buffer)
  }

  @Fn("Describes the state of the current workspace.")
  def describeWorkspace () :Unit = {
    val bb = new BufferBuilder(view.width()-1)
    wspace.describeSelf(bb)

    val hstore = Store.scratch(s"*workspace:${wspace.name}*", buffer.store)
    val hbuf = wspace.createBuffer(hstore, reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  @Fn("Opens the config file for the workspace's info window specifications in a buffer.")
  def editWindowConfig () :Unit = wspace.visitWindowConfig(window)

  /** The history ring for workspace names. */
  protected def nameHistory = wspace.historyRing("workspace-name")

  private val wsvc = env.msvc.service[WorkspaceService]
}
