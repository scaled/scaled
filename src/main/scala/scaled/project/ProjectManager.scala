//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import com.google.common.collect.HashBiMap
import java.io.{File, FileWriter, PrintWriter}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scaled._

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager (metaSvc :MetaService, pluginSvc :PluginService)
    extends AbstractService with ProjectService {

  // maps from id, srcurl to project root for all known projects
  private val byID  = HashBiMap.create[String,File]()
  private val byURL = HashBiMap.create[String,File]()
  // map from root to name for all known projects (name is not necessarily unique)
  private val toName = MMap[File,String]()

  private val mapFile = metaSvc.metaFile("projects.txt")
  readProjectMap() // TODO: set up a file watch on mapFile; reload on change

  // currently resolved projects
  private val projects = MMap[File,Project]() // TODO: use concurrent maps? need we worry?

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

  def projectForId (id :String) = byID.get(id) match {
    case null => None
    case root => Some(projectFor(root))
  }
  def projectForSrcURL (srcURL :String) = byURL.get(srcURL) match {
    case null => None
    case root => Some(projectFor(root))
  }
  def loadedProjects = projects.values.toSeq
  def knownProjects = toName.toSeq

  private def findOpenProject (paths :List[File]) :Option[Project] =
    if (paths.isEmpty) None
    else projects.get(paths.head) orElse findOpenProject(paths.tail)

  private def resolveProject (paths :List[File]) :Option[Project] = {
    def create (pi :(ProjectFinderPlugin,File)) = {
      val (pf, root) = pi
      // println(s"Creating ${pf.name} project in $root")
      val proj = metaSvc.injectInstance(pf.projectClass, List(root))
      projects += (root -> proj)

      // add this project to our all-projects maps, and save them if it's new
      val newID = proj.id.map(id => byID.put(id, root) != root).getOrElse(false)
      val newURL = proj.sourceURL.map(url => byURL.put(url, root) != root).getOrElse(false)
      val newName = toName.put(root, proj.name) != Some(proj.name)
      if (newID || newURL || newName) {
        metaSvc.log(s"New project in '$root', updating '${mapFile.getName}'.")
        writeProjectMap()
      }

      // println(s"Created $proj")
      Some(proj)
    }

    // apply each of our finders to the path tree
    val (iprojs, dprojs) = finders.plugins.flatMap(_.apply(paths)).partition(_._1.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) metaSvc.log(
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

  private def readProjectMap () {
    if (mapFile.exists) try {
      Source.fromFile(mapFile).getLines.foreach { line => line.split("\t") match {
        case Array(rpath, id, url, name) =>
          val root = new File(rpath)
          if (id   != "none") byID.put(id, root)
          if (url  != "none") byURL.put(url, root)
          if (name != "none") toName.put(root, name)
        case _ => metaSvc.log(s"Invalid line in projects.txt: $line")
      }}
    } catch {
      case e :Exception => metaSvc.log(s"Failed to read $mapFile", e)
    }
  }

  private def writeProjectMap () {
    import scala.collection.convert.WrapAsScala._
    val roots = byID.values ++ byURL.values ++ toName.keySet
    val out = new PrintWriter(new FileWriter(mapFile))
    try {
      def orNone (str :String) = if (str == null) "none" else str
      roots foreach { root =>
        val id = byID.inverse.get(root)
        val url = byURL.inverse.get(root)
        val name = toName.getOrElse(root, null)
        if (id != null || url != null || name != null) {
          out.println(s"$root\t${orNone(id)}\t${orNone(url)}\t${orNone(name)}")
        }
      }
    } finally {
      out.close()
    }
  }
}
