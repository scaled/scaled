//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.geometry.Insets
import javafx.scene.control.Label
import javafx.scene.layout.BorderPane

import scala.annotation.tailrec

import reactual.{Future, Promise}

import scaled._
import scaled.major.MiniUI
import scaled.major.MinibufferMode

abstract class MiniOverlay (editor :EditorPane) extends BorderPane {

  getStyleClass.addAll("overpop", "mini")
  setVisible(false)

  val plabel = new Label()
  plabel.maxWidthProperty.bind(widthProperty)
  plabel.setWrapText(true)
  plabel.getStyleClass.add("prompt")
  BorderPane.setMargin(plabel, new Insets(0, 0, 5, 0))
  setTop(plabel)

  val cview = new BufferViewImpl(editor, BufferImpl.scratch("*completions*"), 40, 0)
  val carea = new BufferArea(editor, cview, null) {
    override protected def wasResized (widthChars :Int, heightChars :Int) {
      // we don't save our layout width/height back into our view width/height here because we
      // don't want the completion area to prefer ever larger sizes across uses; the active
      // minibuffer will enforce a monotonically increasing width (for that particular minibuffer
      // invocation) and the completion area can/should grow and shrink in height based on how many
      // completions are displayed
    }
  }

  val ui = new MiniUI() {
    override def setPrompt (prompt :String) = plabel.setText(prompt)
    override def getPrompt = plabel.getText
    override def showCompletions (comps :Seq[String]) {
      if (comps.isEmpty) setBottom(null)
      else {
        cview.buffer.replace(cview.buffer.start, cview.buffer.end, comps.map(new Line(_)))
        cview.width() = comps.map(_.length).max
        cview.height() = comps.length
        setBottom(carea)
        BorderPane.setMargin(carea, new Insets(5, 0, 0, 0))
      }
    }
  }

  /** Called when we clear an active minibuffer. */
  def onClear () :Unit

  /** Displays a minibuffer with the specified mode. */
  def read[R] (mode :String, result :Promise[R], args :List[Any]) :Future[R] = try {
    if (getCenter != null) throw new Exception(
      "Command attempted to use minibuffer while in minibuffer")

    val view = new BufferViewImpl(editor, BufferImpl.scratch("*minibuffer*"), 40, 1)
    val modeArgs = ui :: result :: args
    val disp = new DispatcherImpl(editor, editor.resolver, view, s"mini-$mode", modeArgs)
    val area = new BufferArea(editor, view, disp) {
      override protected def wasResized (widthChars :Int, heightChars :Int) {
        // only persist width; height is unfortunately delivered bogus values due to JavaFX
        // preferred size retardery
        view.width() = widthChars;
      }
    }
    setCenter(area)
    setVisible(true)
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
      editor.emitStatus(e.getMessage)
      result
  }
}
