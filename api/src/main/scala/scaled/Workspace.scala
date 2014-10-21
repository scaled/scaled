//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import java.nio.file.Path
import scaled.util.Reffed

/** Coordinates metadata and operations for a workspace. A workspace defines an environment tailored
  * to working on a particular software artifact. Each workspace is like a separate Scaled
  * installation, customized specially for the desired task. Multiple editor windows may be open for
  * a single workspace.
  *
  * The core editor uses workspaces as a configuration grouping, i.e. one can customize modes, etc.
  * specially for a particular workspace.
  *
  * A workspace also serves as a useful abstraction for external services. For example the project
  * services use workspaces to collect a group of related projects, resolve inter-project
  * dependencies specially for projects that are added to the workspace, and to provide
  * workspace-wide searching and navigation functionality.
  */
abstract class Workspace extends Reffed {

  /** The user supplied name for this workspace. */
  val name :String

  /** The root directory in which metadata is stored for this workspace. */
  val root :Path

  /** State local to this workspace. */
  val state :State = new State(
    // pre-populate our state with our workspace config scope
    State.init(Config.Scope("workspace", root, None)))

  /** Returns whether this is the default (auto-created) workspace. */
  def isDefault = name == Workspace.DefaultName

  // TODO: create new editor (with geometry?)

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
