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

class Main extends Application {

  override def start (stage :Stage) {
    // TODO: open a pane/tab for each file passed on the command line
    val buff = BufferImpl.fromFile(new File(getParameters.getRaw.get(0)))
    val editor = new EditorPane(buff)

    // TODO: get stage size from config
    val scene = new Scene(editor)
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
