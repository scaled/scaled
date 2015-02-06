//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.nio.file.Path
import scaled.util.Close

/** Coordinates metadata and operations for a workspace. A workspace defines an environment
  * tailored to working on a particular software artifact. Each workspace is like a separate Scaled
  * installation, customized specially for the desired task. Multiple editor windows may be open
  * for a single workspace.
  *
  * The core editor uses workspaces as a configuration grouping, i.e. one can customize modes, etc.
  * specially for a particular workspace.
  *
  * A workspace also serves as a useful abstraction for external services. For example the project
  * services use workspaces to collect a group of related projects, resolve inter-project
  * dependencies specially for projects that are added to the workspace, and to provide
  * workspace-wide searching and navigation functionality.
  */
abstract class Workspace {

  /** The user supplied name for this workspace. */
  val name :String

  /** The root directory in which metadata is stored for this workspace. */
  val root :Path

  /** An event bus for workspace-scoped event dispatch. */
  val events :EventBus = new EventBus()

  /** State local to this workspace. */
  val state :State = new State(
    // pre-populate our state with our workspace config scope
    State.init(Config.Scope("workspace", root, None)))

  /** A bag of closeables that will be closed when this workspace hibernates. When all of a
    * workspace's windows are closed, it hibernates, i.e. unloads everything it can from memory.
    * Entities can participate in the workspace lifecycle by coming to life when
    * [Editor.workspaceOpened] is emitted and then registering to be closed via this bag. */
  val toClose = Close.bag()

  /** A signal via which a background service can emit a status message which will be displayed by
    * all windows open in this workspace. */
  val statusMsg = Signal[String](editor.exec.uiExec)

  /** The global editor in which this workspace is contained. */
  def editor :Editor

  /** Global configuration (potentially customized for this workspace).
    * Keys are defined in [[EditorConfig]]. */
  def config :Config

  /** Returns whether this is the default (auto-created) workspace. */
  def isDefault = name == Workspace.DefaultName

  /** Returns all windows owned by this workspace. */
  def windows :SeqV[Window]

  /** Returns all buffers open in this workspace, in order of most recent activation. */
  def buffers :SeqV[Buffer]

  /** Returns the [[Window#Frame]] displaying `buffer` or `None`. */
  def frameForBuffer (buffer :Buffer) :Option[Window#Frame] =
    windows.flatMap(_.frames).find(_.view.buffer == buffer)

  /** Returns the [[Window]] displaying `buffer` or `None`. */
  def windowForBuffer (buffer :Buffer) :Option[Window] =
    windows.find(_.frames.exists(_.view.buffer == buffer))

  /** A signal emitted when a buffer is opened. */
  def bufferOpened :SignalV[RBuffer]

  /** Creates a buffer named `name` and with the specified initial `state`.
    * @param reuse requests reuse of an existing buffer with the same name. If a buffer named
    * `name` exists, it will be returned directly (as is, so be careful you're not getting an
    * unexpected buffer in this case). Otherwise in the event of name collision, a fresh buffer
    * name will be generated from `name` by appending <N> to the name with increasing values of N
    * until an unused name is obtained. */
  def createBuffer (name :String, state :List[State.Init[_]] = Nil, reuse :Boolean = false) :Buffer

  /** Opens a buffer for `file` in this workspace. If a buffer is already open for `file` it will
    * be returned instead. */
  def openBuffer (file :Store) :Buffer

  /** Opens a new window in this workspace. The window will not become visible until the caller
    * instructs it to visit a buffer.
    * @param geom optional gemoetry for the window. If none is provided, the geometry will be
    * determined by the user's configurtion. */
  def openWindow (geom :Option[Geometry]) :Window

  /** Adds `path` to this workspace's list of hint paths. A hint path is used to trigger the
    * selection of this workspace when Scaled is first started. If the file being edited is in a
    * subdirectory of a workspace's hint path, that workspace will be automatically activated. */
  def addHintPath (path :Path) :Unit

  /** Removes `path` from this workspace's list of hint paths. */
  def removeHintPath (path :Path) :Unit
}

/** Static [[Workspace]] stuffs. */
object Workspace {

  /** The name of the default workspace. */
  val DefaultName = "Default"

  /** The history ring for file names (find-file, write-file, etc.). */
  def fileHistory (ws :Workspace) = historyRing(ws, "file")
  /** The history ring for buffer names (switch-to-buffer, kill-buffer, etc.). */
  def bufferHistory (ws :Workspace) = historyRing(ws, "buffer")
  /** The history ring for replace fns (replace-string, replace-regexp, query-replace, etc.). */
  def replaceHistory (ws :Workspace) = historyRing(ws, "replace")
  /** The history ring used for mode names. */
  def modeHistory (ws :Workspace) = historyRing(ws, "mode")
  /** The history ring used for fns. */
  def fnHistory (ws :Workspace) = historyRing(ws, "fn")
  /** The history ring used for config var names. */
  def varHistory (ws :Workspace) = historyRing(ws, "var")
  /** The history ring used for config var values. */
  def setVarHistory (ws :Workspace) = historyRing(ws, "set-var")

  /** Returns the (editor-wide) history ring with the specified name. The ring will be created
    * on-demand. Note the history ring names above, which are used by Scaled. */
  def historyRing (ws :Workspace, name :String) = Mutable.getOrPut(
    Rings(ws.state), name, new Ring(ws.config(EditorConfig.historySize)) {
      override def toString = s"$name-history"
    })
}

/** Provides workspace services. */
@Service(name="workspace", impl="impl.WorkspaceManager",
         desc="Handles creation and management of workspaces.")
trait WorkspaceService {

  /** Returns the list of known concrete workspaces. */
  def list :Seq[String]

  /** Creates and [[open]]s a new concrete workspace named `name`.
    * @throws `FeedbackException` if a workspace named `name` already exists. */
  def create (name :String) :Unit

  /** Resolves the specified workspace and creates a new editor with it as the active workspace.
    * This method doesn't return anything because the expectation is that the user will switch to
    * the new editor and start doing things. */
  def open (name :String) :Unit
}
