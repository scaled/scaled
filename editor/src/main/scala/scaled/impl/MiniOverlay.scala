//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.sun.javafx.tk.Toolkit
import javafx.geometry.Insets
import javafx.scene.control.{Label, Tooltip}
import javafx.scene.layout.BorderPane
import reactual.{Future, Promise, ValueV}
import scala.annotation.tailrec
import scaled._
import scaled.major.{MiniUI, MinibufferMode}
import scaled.util.Error

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
        val fcomps = formatCompletions(comps)
        cview.buffer.replace(cview.buffer.start, cview.buffer.end, fcomps.map(Line.apply))
        cview.width() = fcomps.map(_.length).max
        cview.height() = fcomps.length
        setBottom(carea)
        BorderPane.setMargin(carea, new Insets(5, 0, 0, 0))
      }
    }
  }

  /** Called when we clear an active minibuffer. */
  def onClear () :Unit

  /** Displays a minibuffer with the specified mode. */
  def read[R] (mode :String, result :Promise[R], args :List[Any]) :Future[R] = try {
    if (getCenter != null) throw Error.feedback(
      "Command attempted to use minibuffer while in minibuffer")

    val view = new BufferViewImpl(editor, BufferImpl.scratch("*minibuffer*"), 40, 1)
    val modeArgs = ui :: result :: args
    val disp = new DispatcherImpl(editor, editor.resolver, view, ModeLine.Noop,
                                  s"mini-$mode", modeArgs)
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
      editor.emitError(e)
      result
  }

  private def formatCompletions (comps :Seq[String]) :Seq[String] = {
    // our completion area might not have been shown yet, so we have to get the font width
    // from the prompt label and then do the pixel to char max width math ourselves; sigh...
    val fm = Toolkit.getToolkit.getFontLoader.getFontMetrics(plabel.getFont)
    val charWidth = fm.computeStringWidth("W")
    // add a gap of two chars between columns
    val colWid = comps.map(_.length).max+2
    // leave two columns on either side to accommodate padding
    val availWid = (getMaxWidth / charWidth).toInt - 4
    val cols = availWid / colWid
    if (cols <= 1) comps
    else comps.grouped(cols).map(_.map(s"%-${colWid}s".format(_)).mkString).toSeq
  }
}
