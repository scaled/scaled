//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman

import java.io.File

/** The main command line entry point for the Scaled Package Manager. */
object Main {
  import scala.collection.convert.WrapAsScala._

  val usage = """
Usage: spam <command>

where <command> is one of:

  install [pkg-url]                  installs package at pkg-url, and its dependencies
  list                               lists all installed packages
  info [pkg-name | --all]            prints detailed info on pkg-name (or all packages)
  depends pkg-name                   prints the dependency tree for pkg-name
  run pkg-name classname [arg ...]   runs classname from pkg-name with args
"""

  val pacman = PackageManager
  val out = new Printer(System.out)

  def main (args :Array[String]) {
    // we'll introduce proper arg parsing later; for now KISS
    args match {
      case Array("install", pkgURL) =>
        out.println("TODO!")

      case Array("list") =>
        out.println("Installed packages:")
        out.printCols(pacman.packages map { pkg => (pkg.info.name, pkg.info.source.toString) },
                      "No packages found.")

      case Array("info", name) =>
        if (name == "--all") for (pkg <- pacman.packages) {
          out.println("------------------------------------------------------------")
          printInfo(pkg)
          out.println("")
        } else onPackage(name)(printInfo)

      case Array("depends", pkgName) =>
        onPackage(pkgName) { _.loader.dependTree.dump() }

      case Array("run", pkgName, classname, args @ _*) =>
        onPackage(pkgName) { pkg =>
          val clazz = pkg.loader.loadClass(classname)
          clazz.getMethod("main", classOf[Array[String]]).invoke(null, args.toArray)
        }

      case _ =>
        fail(usage)
    }
  }

  def onPackage (name :String)(fn :Package => Unit) {
    pacman.packages.find(_.info.name == name) match {
      case None      => fail(s"Unknown package: $name")
      case Some(pkg) => fn(pkg)
    }
  }

  def fail (msg :String) {
    System.err.println(msg)
    System.exit(255)
  }

  def printInfo (pkg :Package) {
    out.println(s"Package: ${pkg.info.name}")
    out.printCols(Seq("Install:" -> pkg.info.root.toString,
                      "Source:"  -> pkg.info.source.toString,
                      "Version:" -> pkg.info.version,
                      "Descrip:" -> pkg.info.descrip,
                      "Web URL:" -> pkg.info.weburl,
                      "License:" -> pkg.info.license,
                      "Src Dir:" -> pkg.info.srcdir,
                      "Bin Dir:" -> pkg.info.bindir), "", 1)
  }
}
