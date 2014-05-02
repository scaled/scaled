//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scaled._

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager (metaSvc :MetaService, pluginSvc :PluginService)
    extends AbstractService with ProjectService {

  private val byRoot = MMap[File,Project]() // TODO: use concurrent maps? need we worry?
  private val byName = MMap[String,Project]()
  private val byID   = MMap[String,Project]()
  private val byURL  = MMap[String,Project]()

  private val finders = pluginSvc.resolvePlugins[ProjectFinderPlugin]("project-finder")

  // TODO: have projects export names, allow switching between projects by names
  // (persist project name to project root, once we see one)
  // then also finding files in other projects? (i.e. C-x C-f -> codex:somefile)

  def didStartup () {
    // TODO!
  }

  def willShutdown () {
    // TODO!
  }

  def projectFor (file :File) = {
    // compute the path tree from this file to the file system root
    val paths = parents(file.getParentFile)
    // either we have an open project, we can resolve one, or we use the last ditch project
    findOpenProject(paths) orElse resolveProject(paths) getOrElse FileProject.lastDitch(paths.head)
  }

  def loadedProjects = byRoot.values.toSeq

  // TODO: store known projects somewhere
  def knownProjects = byRoot.values.map(p => (p.id, p.sourceURL, p.root)).toSeq

  private def findOpenProject (paths :List[File]) :Option[Project] =
    if (paths.isEmpty) None
    else byRoot.get(paths.head) orElse findOpenProject(paths.tail)

  private def resolveProject (paths :List[File]) :Option[Project] = {
    def create (pi :(ProjectFinderPlugin,File)) = {
      val (pf, root) = pi
      // println(s"Creating ${pf.name} project in $root")
      val proj = metaSvc.injectInstance(pf.projectClass, List(root))
      // map the project six ways to sunday
      byRoot += (root -> proj)
      byName += (proj.name -> proj)
      proj.id map { id => byID += (id -> proj) }
      proj.sourceURL map { url => byURL += (url -> proj) }
      // println(s"Created $proj")
      Some(proj)
    }

    // apply each of our finders to the path tree
    val (iprojs, dprojs) = finders.plugins.flatMap(_.apply(paths)).partition(_._1.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) println(
        s"Multiple intelligent project matches: ${iprojs.mkString(" ")}")
      create(iprojs.head)
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dprojs.isEmpty) {
      var deep = dprojs.head ; var dps = dprojs.toList.tail
      while (!dps.isEmpty) {
        if (dps.head._2.getPath.length > deep._2.getPath.length) deep = dps.head
        dps = dps.tail
      }
      create(deep)
    }
    else None
  }

  @tailrec private def parents (file :File, accum :List[File] = Nil) :List[File] =
    file.getParentFile match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}
