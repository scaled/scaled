package scaled.impl

import java.io.File

import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.BorderPane
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage

import scaled.Editor

class Main extends Application {

  val editor = new Editor {
    override val killRing = new KillRingImpl(40) // TODO: get size from config
    override def showURL (url :String) = getHostServices.showDocument(url)
    override def exit (code :Int) = sys.exit(code) // TODO: cleanup?
  }

  override def start (stage :Stage) {
    // TODO: open a pane/tab for each file passed on the command line
    val buff = BufferImpl.fromFile(new File(getParameters.getRaw.get(0)))
    val epane = new EditorPane(editor, buff)

    // TODO: get stage size from config
    val scene = new Scene(epane)
    // TODO: how to support themes, etc.?
    scene.getStylesheets().add(getClass.getResource("/scaled.css").toExternalForm)
    stage.setScene(scene)
    stage.setTitle("Scaled")
    stage.show()
  }
}

object Main {

  def main (args :Array[String]) {
    if (args.isEmpty) {
      println("Usage: scaled [file ...]")
      sys.exit(255)
    } else Application.launch(classOf[Main], args :_*)
  }
}
