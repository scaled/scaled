//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scaled._

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager extends AbstractService with ProjectService {
  import ProjectManager._

  private val projects = MMap[File,Project]() // TODO: use concurrent map? need we worry?

  def didStartup () {
    // TODO!
  }

  def willShutdown () {
    // TODO!
  }

  def projectFor (file :File) = {
    // compute the path tree from this file to the file system root
    val paths = parents(file.getParentFile)
    // either we have an open project, we can resolve one, or we use the default
    findOpenProject(paths) orElse resolveProject(paths) getOrElse defaultProject
  }

  // TODO: the finders need to come from some sort of package plugin mechanism
  private val finders = List(FileProject.gitFinder, FileProject.hgFinder)

  private def findOpenProject (paths :List[File]) :Option[Project] =
    if (paths.isEmpty) None
    else projects.get(paths.head) orElse findOpenProject(paths.tail)

  private def resolveProject (paths :List[File]) :Option[Project] = {
    def create (pi :(ProjectFinder,File)) = {
      val (pf, root) = pi
      println(s"Creating ${pf.name} project in $root")
      val proj = pf.createProject(root)
      projects += (root -> proj)
      Some(proj)
    }
    // apply each of our finders to the path tree
    val (iprojs, dprojs) = finders.flatMap(_.apply(paths)).partition(_._1.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) println(
        s"Multiple intelligent project matches: ${iprojs.mkString(" ")}")
      create(iprojs.head)
    }
    // if there are any non-intelligent project matches, use one
    else if (!dprojs.isEmpty) create(dprojs.head)
    else None
  }

  @tailrec private def parents (file :File, accum :List[File] = Nil) :List[File] =
    file.getParentFile match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}

object ProjectManager {

  /** The project used if we absolutely cannot find another project. */
  val defaultProject = new Project() {
    override val fileCompleter = Completer.file
  }
}
