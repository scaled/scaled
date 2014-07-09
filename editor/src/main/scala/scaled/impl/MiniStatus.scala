//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.tk.Toolkit
import javafx.geometry.Insets
import javafx.scene.control.{Label, Tooltip}
import javafx.scene.layout.BorderPane
import reactual.{Future, Promise, ValueV}
import scala.annotation.tailrec
import scaled._
import scaled.major.{MiniUI, MinibufferMode}
import scaled.util.Errors

abstract class MiniStatus (editor :EditorPane) extends BorderPane with Minibuffer {

  getStyleClass.addAll("status")
  setVisible(false)

  val plabel = new Label()
  plabel.maxWidthProperty.bind(widthProperty)
  plabel.setWrapText(true)
  plabel.getStyleClass.add("prompt")
  BorderPane.setMargin(plabel, new Insets(0, 5, 0, 0))
  setLeft(plabel)

  val ui = new MiniUI() {
    override def setPrompt (prompt :String) = plabel.setText(prompt)
    override def getPrompt = plabel.getText
    override def showCompletions (comps :Seq[String]) {} // not supported presently
  }

  /** Called when wish this minibuffer to be made visible. */
  def onShow () {
    setVisible(true)
    toFront()
  }

  /** Called when we clear an active minibuffer. */
  def onClear () :Unit

  override def apply[R] (mode :String, result :Promise[R], args :Any*) :Future[R] = try {
    if (getCenter != null) throw Errors.feedback(
      "Command attempted to use minibuffer while in minibuffer")

    val view = new BufferViewImpl(editor, BufferImpl.scratch("*minibuffer*"), 40, 1)
    val modeArgs = ui :: result :: args.toList
    val disp = new DispatcherImpl(editor, editor.resolver, view, ModeLine.Noop,
                                  s"mini-$mode", modeArgs, Nil)
    val area = new BufferArea(editor, view, disp) {
      override protected def wasResized (widthChars :Int, heightChars :Int) {
        // only persist width; height is unfortunately delivered bogus values due to JavaFX
        // preferred size retardery
        view.width() = widthChars;
      }
    }
    setCenter(area)
    onShow()
    area.requestFocus()
    result onComplete { _ =>
      ui.setPrompt("")
      ui.showCompletions(Seq())
      view.popup.clear() // clear any active popup
      disp.dispose()
      setCenter(null)
      setVisible(false)
      onClear()
    }

  } catch {
    case e :Exception =>
      result.fail(e)
      editor.emitError(e)
      result
  }
}
