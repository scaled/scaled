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
    wspace.openWindow(None).focus.visit(buffer)
  }

  @Fn("Describes the state of the current workspace.")
  def describeWorkspace () {
    val bb = new BufferBuilder(view.width()-1)
    bb.addHeader("Workspace")
    bb.addKeysValues("Name: " -> wspace.name,
                     "Root: " -> wspace.root.toString,
                     "Buffers: " -> wspace.buffers.size.toString)
    wspace.state.describeSelf(bb)

    bb.addHeader("Buffers")
    wspace.buffers.foreach { buf =>
      bb.addSubHeader(buf.name)
      bb.addKeysValues("Store: " -> buf.store.toString,
                       "Length: " -> buf.offset(buf.end).toString)
      buf.state.describeSelf(bb)
    }

    val hstore = Store.scratch(s"*workspace:${wspace.name}*", buffer.store)
    val hbuf = wspace.createBuffer(hstore, reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  /** The history ring for workspace names. */
  protected def nameHistory = Workspace.historyRing(wspace, "workspace-name")

  private val wsvc = env.msvc.service[WorkspaceService]
}
