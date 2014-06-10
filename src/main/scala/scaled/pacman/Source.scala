//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman

import java.net.{MalformedURLException, URL}

case class Source (vcs :Source.VCS, url :URL) extends Package.Id {
  override def toString = s"$vcs:$url"
}

object Source {

  /** Enumerates our VCSes. */
  sealed trait VCS
  case object Git extends VCS {
    override def toString = "git"
  }
  case object Mercurial extends VCS {
    override def toString = "hg"
  }
  case object Subversion extends VCS {
    override def toString = "svn"
  }

  def parse (text :String) :Source = text split(":", 2) match {
    case Array(vcs, url) => parse(vcs, url)
    case _               => throw new IllegalArgumentException(s"Invalid VCS URL: $text")
  }

  def parse (vcs :String, url :String) = vcs match {
    case "git" => Source(Git, new URL(url))
    case "hg"  => Source(Mercurial, new URL(url))
    case "svn" => Source(Subversion, new URL(url))
    case _     => throw new IllegalArgumentException(s"Unsupported VCS: $vcs (url=$url)")
  }
}
