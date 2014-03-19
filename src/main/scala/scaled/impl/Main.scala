package scaled.impl

import java.io.File

import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.Stage

import scala.collection.JavaConversions._

class Main extends Application {

  override def start (stage :Stage) {
    val epane = new EditorPane(this, stage)
    // open a pane/tab for each file passed on the command line
    getParameters.getRaw foreach { p =>
      // TODO: should Editor just take a path and do this massaging internally? or should we export
      // a public Files utility class and recommend callers use that? meh...
      val f = new File(p)
      epane.newBuffer(if (f.exists || f.isAbsolute) f else new File(cwd(), p))
    }

    val scene = new Scene(epane)
    // TODO: how to support themes, etc.?
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    stage.setScene(scene)
    stage.show()
  }
}

object Main {

  def main (args :Array[String]) {
    Application.launch(classOf[Main], args :_*)
  }
}
