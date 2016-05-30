//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.tk.Toolkit
import javafx.geometry.Insets
import javafx.scene.control.{Label, Tooltip}
import javafx.scene.layout.BorderPane
import scaled._
import scaled.major.{MiniUI, MinibufferMode}
import scaled.util.Errors

abstract class MiniStatus (window :WindowImpl) extends BorderPane with Minibuffer {

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
    override def showCompletions (comps :SeqV[String]) {} // not supported presently
  }

  /** Called to check whether we can show this minibuffer.
    * Should throw a feedback exception (with explanation) if showing is not currently allowed. */
  def willShow () :Unit

  /** Called when this minibuffer is made visible. */
  def onShow () :Unit

  /** Called when this minibuffer is cleared. */
  def onClear () :Unit

  override def apply[R] (mode :String, args :Any*) :Future[R] = {
    val result = window.exec.uiPromise[R]
    try {
    willShow() // make sure it's OK to activate ourselves

    val buffer = BufferImpl.scratch("*minibuffer*")
    val view = new BufferViewImpl(buffer, 40, 1)
    val modeArgs = ui :: result :: List.copyOf(args)
    val disp = new DispatcherImpl(window, window.resolver(null, buffer), view, ModeLine.Noop,
                                  s"mini-$mode", modeArgs, Nil) {
      override protected def fallbackToText = false
    }
    setCenter(disp.area)
    setVisible(true)
    toFront()
    onShow()
    disp.area.requestFocus()
    result onComplete { _ =>
      ui.setPrompt("")
      ui.showCompletions(Seq())
      view.popup.clear() // clear any active popup
      disp.dispose(true)
      setCenter(null)
      setVisible(false)
      onClear()
    }

  } catch {
    case e :Exception =>
      result.fail(e)
      window.emitError(e)
  }
    result
  }
}
