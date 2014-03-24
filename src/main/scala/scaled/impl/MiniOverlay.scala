//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.control.Label
import javafx.scene.layout.BorderPane

import scala.annotation.tailrec

import reactual.{Future, Promise}

import scaled._
import scaled.major.MiniPrompt
import scaled.major.MinibufferMode

abstract class MiniOverlay (editor :EditorPane) extends BorderPane {

  getStyleClass.addAll("overpop", "mini")

  val bufName = "*minibuffer*"

  val plabel = new Label()
  plabel.maxWidthProperty.bind(widthProperty)
  plabel.setWrapText(true)
  plabel.getStyleClass.add("prompt")
  setTop(plabel)

  val prompt = new MiniPrompt() {
    override def set (prompt :String) = plabel.setText(prompt)
    override def get = plabel.getText
  }

  /** Called when we clear an active minibuffer. */
  def onClear () :Unit

  /** Displays a minibuffer with the specified mode. */
  def read[R] (mode :String, result :Promise[R], args :List[Any]) :Future[R] = try {
    if (getCenter != null) throw new Exception(
      "Command attempted to use minibuffer while in minibuffer")

    // TODO: proper config
    val config = new ConfigImpl()
    val view = new BufferViewImpl(editor, BufferImpl.scratch(bufName), 40, 1)
    val modeArgs = prompt :: result :: args
    val disp = new DispatcherImpl(editor, view) {
      override def createMode () = {
        editor.resolver.resolveMajor(s"mini-$mode", config, view, this, modeArgs) match {
          case Some(mmode :MinibufferMode) => mmode
          case Some(mmode) => throw new Exception(s"$mode is not a MinibufferMode")
          case None => throw new Exception(s"Unknown minibuffer mode $mode")
        }
      }
    }
    val area = new BufferArea(editor, view, disp)
    setCenter(area)
    setVisible(true)
    area.requestFocus()
    result onComplete { _ =>
      prompt.set("")
      view.popup.clear() // clear any active popup
      setCenter(null)
      setVisible(false)
      onClear()
    }

  } catch {
    case e :Exception =>
      result.fail(e)
      editor.emitStatus(e.getMessage)
      result
  }
}
