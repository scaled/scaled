//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.TreeMap
import scaled.Completer

/** A simple project used when we can't identify anything other than the root directory of the
  * project. This will be used if we see a `.git`, `.hg`, etc. directory or some other indicator
  * of the root of a project.
  */
class FileProject (root :File, ignores :Set[String]) extends Project {

  private class Dir (dir :File) {
    var files = Set[File]()
    var dirs = Set[File]()
    var lastRefresh = 0L

    def refresh () {
      val lm = dir.lastModified
      if (lm > lastRefresh) {
        lastRefresh = lm
        val (nd, nf) = dir.listFiles.partition(_.isDirectory)
        val nfiles = nf.toSet
        if (files != nfiles) {
          files = nfiles
          _allFiles = null
        }
        val ndirs = nd.filterNot(ignore).map(_.getCanonicalFile).toSet
        if (ndirs != dirs) {
          _allFiles = null
          // remove directories that have gone away
          (dirs -- ndirs) foreach { dirMap -= _ }
          // add new directories
          val newdirs = (ndirs -- dirs)
          newdirs foreach { d => dirMap += (d -> new Dir(d)) }
          // and refresh any directories that have changed
          (ndirs -- newdirs) map(dirMap) foreach { _.refresh() }
          // finally update our cached directories
          dirs = ndirs
        }
        // println(s"Rebuilt $dir (files=${files.size} dirs=${dirs.size})")
      }
      // refresh our children
      dirs map(dirMap) foreach { _.refresh() }
    }
  }
  private val dirMap = MMap[File,Dir](root -> new Dir(root))

  private var _allFiles :TreeMap[String,File] = _
  private def allFiles = {
    if (_allFiles == null) {
      _allFiles = TreeMap[String,File]() ++ dirMap.values.flatMap(_.files).map(
        f => (f.getName -> f))
      // println(s"Rebuilt all files map (size: ${_allFiles.size})")
    }
    _allFiles
  }

  val fileCompleter = new Completer[File]() {
    def apply (prefix :String) /*:SortedMap[String,File]*/ = {
      dirMap(root).refresh()
      allFiles.filterKeys(_ startsWith prefix)
    }
  }

  protected def ignore (dir :File) :Boolean = ignores(dir.getName)
}

object FileProject {

  /** The directories ignored by file projects. */
  val fileIgnores = ProjectFinder.stockIgnores // TODO ++ others?

  abstract class FileProjectFinder (nm :String) extends ProjectFinder(nm, false) {
    def createProject (root :File) = new FileProject(root, fileIgnores)
  }

  /** Creates file projects rooted at .git directories. */
  val gitFinder = new FileProjectFinder("git") {
    def checkRoot (root :File) = if (new File(root, ".git").isDirectory()) 1 else -1
  }

  /** Creates file projects rooted at .hg directories. */
  val hgFinder = new FileProjectFinder("mercurial") {
    def checkRoot (root :File) = if (new File(root, ".hg").isDirectory()) 1 else -1
  }

  /** Creates file projects rooted at the highest .svn directory. */
  val svnFinder = new FileProjectFinder("subversion") {
    def checkRoot (root :File) = if (new File(root, ".svn").isDirectory()) 0 else -1
  }
}
