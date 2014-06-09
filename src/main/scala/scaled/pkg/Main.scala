//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pkg

/** The main command line entry point for the Scaled Package Manager. */
object Main {
  import scala.collection.convert.WrapAsScala._

  val usage = """
Usage: spam <command>

where <command> is one of:

  install [pkg-url]                  installs package at pkg-url, and its dependencies
  list                               lists all installed packages
  info [pkg-name | --all]            prints detailed info on pkg-name (or all packages)
  run pkg-name classname [arg ...]   runs classname from pkg-name with args
"""

  lazy val pacman = new PackageManager()
  val out = new Printer(System.out)

  def main (args :Array[String]) {
    // we'll introduce proper arg parsing later; for now KISS
    args match {
      case Array("install", pkgURL) =>

      case Array("list") =>
        out.println("Installed packages:")
        out.printCols(pacman.packages map { pkg => (pkg.info.name, pkg.info.srcurl) },
                      "No packages found.")

      case Array("info", name) =>
        if (name == "--all") for (pkg <- pacman.packages) {
          out.println("------------------------------------------------------------")
          printInfo(pkg)
          out.println("")
        } else pacman.packages.find(_.info.name == name) match {
          case None => out.println(s"Unknown package: $name")
          case Some(pkg) => printInfo(pkg)
        }

      case Array("run", pkgName, className, args @ _*) =>
      case _ =>
        System.err.println(usage)
        System.exit(255)
    }
  }

  def printInfo (pkg :Package) {
    out.println(s"Package: ${pkg.info.name}")
    out.printCols(Seq("Install:" -> pkg.info.root.toString,
                      "Version:" -> pkg.info.version,
                      "Descrip:" -> pkg.info.descrip,
                      "Web URL:" -> pkg.info.weburl,
                      "Src URL:" -> pkg.info.srcurl,
                      "License:" -> pkg.info.license,
                      "Src Dir:" -> pkg.info.srcdir,
                      "Bin Dir:" -> pkg.info.bindir), "", 1)
    def print0 (header :String, info :Iterable[(String,String)]) {
      if (info.iterator.hasNext) {
        out.println("")
        out.println(header)
        out.printCols(info, "")
      }
    }
    def print1 (header :String, info :Iterable[java.util.Map.Entry[String,String]]) {
      print0(header, info.map(e => (e.getKey, e.getValue)))
    }
    print0("Major modes:", pkg.majors)
    print0("Minor modes:", pkg.minors)
    print0("Services:", pkg.services)
    print1("Plugins:", pkg.plugins.entries)
    print1("Patterns:", pkg.patterns.entries)
    print1("Interps:", pkg.interps.entries)
    print1("Minor tags:", pkg.minorTags.entries)
  }
}
