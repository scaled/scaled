package scaled.canvas

import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.StackPane
import javafx.stage.Stage

class Main extends Application {

  override def start (stage :Stage) {
    // a simple hello world for now to ensure that all the build bits are properly wired
    stage.setTitle("Scaled Hello World")
    val root = new StackPane()
    val btn = new Button()
    btn.setText("Hello hello")
    btn.setOnAction(new EventHandler[ActionEvent]() {
      override def handle (event :ActionEvent) {
        println("Hello yourself")
      }
    })
    root.getChildren.add(btn)
    stage.setScene(new Scene(root, 300, 250))
    stage.show()
  }
}

object Main {

  def main (args :Array[String]) {
    Application.launch(classOf[Main], args :_*)
  }
}
