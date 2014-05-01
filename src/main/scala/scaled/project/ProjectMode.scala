//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.major.EditingMode

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {

  // TODO
}

/** A minor mode which provides fns for interacting with project files and services.
  *
  * Some stock key bindings are also redirected toward project-centric versions, for example
  * `C-x C-f` is rerouted to `find-file-in-project`. Where possible, the original fns are exposed
  * via slightly varied key bindings.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="project",
       tags=Array("project"),
       desc="""A minor mode that provides project-centric fns.""")
class ProjectMode (env :Env, psvc :ProjectService, major :EditingMode) extends MinorMode(env) {

  // TODO: it's possible that our buffer's file could change and become part of a new project;
  // do we really want to handle that crazy case?
  val project :Project = psvc.projectFor(buffer.file).reference(buffer)

  override def configDefs = ProjectConfig :: super.configDefs
  override def keymap = Seq(
    "C-x C-f" -> "find-file-in-project",
    // TODO: this doens't work, we need to wire up major:find-file to route to major mode fn
    // "S-C-x S-C-f" -> "find-file"
    "S-C-x S-C-f" -> "find-file-default"
  )

  override def dispose () {
    super.dispose()
    project.release(buffer)
  }

  //
  // FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def findFileInProject () {
    editor.miniRead(
      s"Find file in project (${project.name}):", "", project.fileHistory, project.fileCompleter
    ) onSuccess editor.visitFile
  }

  @Fn("TEMP: forwards find-file to major mode")
  def findFileDefault () :Unit = major.findFile()
}
