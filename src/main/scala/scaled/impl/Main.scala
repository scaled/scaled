package scaled.impl

import java.io.File
import java.util.concurrent.Executors
import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.Stage
import scala.collection.JavaConversions._
import scaled.EditorConfig

class Main extends Application {

  /** An executor service for great concurrency. */
  val exec = Executors.newFixedThreadPool(4) // TODO: config

  // locate and create our metadata dir
  val homeDir = new File(System.getProperty("user.home"))
  val metaDir = Filer.requireDir(locateMetaDir)

  val watchMgr = new WatchManager()
  val pkgMgr = new pkg.PackageManager(this)
  val cfgMgr = new ConfigManager(this)

  override def start (stage :Stage) {
    val epane = new EditorPane(this, stage)
    // open a pane/tab for each file passed on the command line
    getParameters.getRaw foreach { p =>
      // TODO: should Editor just take a path and do this massaging internally? or should we export
      // a public Files utility class and recommend callers use that? meh...
      val f = new File(p)
      epane.visitFile(if (f.exists || f.isAbsolute) f else new File(cwd(), p))
    }

    val scene = new Scene(epane)
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    cfgMgr.editorConfig.value(EditorConfig.viewLeft) onValueNotify { x =>
      if (x >= 0) stage.setX(x)
    }
    cfgMgr.editorConfig.value(EditorConfig.viewTop) onValueNotify { y =>
      if (y >= 0) stage.setY(y)
    }
    stage.setScene(scene)
    stage.show()
  }

  // TODO: platform specific app dirs
  private def locateMetaDir :File =
    (homeDir /: Seq("Library", "Application Support", "Scaled"))(new File(_, _))
}

object Main {

  def main (args :Array[String]) {
    Application.launch(classOf[Main], args :_*)
  }
}
