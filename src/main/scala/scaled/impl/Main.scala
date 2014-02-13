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
    // a simple hello world for now to ensure that all the build bits are properly wired
    stage.setTitle("Scaled Hello World")
    val root = new BorderPane()

    // TODO: open a tab? for each file passed on the command line
    val buff = BufferImpl.fromFile(new File(getParameters.getRaw.get(0)))
    val code = new CodeArea(buff)
    root.setCenter(code)

    // TODO: status bar, command entry area, etc.
    stage.setScene(new Scene(root, 640, 480))
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
