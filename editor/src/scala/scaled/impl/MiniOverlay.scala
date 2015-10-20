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

abstract class MiniOverlay (window :WindowImpl) extends BorderPane with Minibuffer {

  getStyleClass.addAll("overpop", "mini")
  setVisible(false)

  val plabel = new Label()
  plabel.maxWidthProperty.bind(widthProperty)
  plabel.setWrapText(true)
  plabel.getStyleClass.add("prompt")
  BorderPane.setMargin(plabel, new Insets(0, 0, 5, 0))
  setTop(plabel)

  val cview = new BufferViewImpl(BufferImpl.scratch("*completions*"), 40, 0)
  val carea = new BufferArea(cview, null) {
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
    override def showCompletions (comps :SeqV[String]) {
      if (comps.isEmpty) setBottom(null)
      else {
        // we have approximately the bottom two thirds of the window for completions
        val maxComps = ((2*window.getHeight/3)/plabel.getHeight-1).toInt
        val fcomps = formatCompletions(comps)
        val tcomps = if (fcomps.size <= maxComps) fcomps else fcomps.take(maxComps-1)
        cview.buffer.replace(cview.buffer.start, cview.buffer.end, tcomps.map(Line.apply))
        if (tcomps.size < fcomps.size) {
          cview.buffer.split(cview.buffer.end)
          cview.buffer.insert(cview.buffer.end, Line(s"...(${comps.size} total matches)..."))
        }
        cview.point() = Loc(0, 0)
        cview.width() = tcomps.map(_.length).max
        cview.height() = math.min(maxComps, fcomps.length)
        setBottom(carea)
        BorderPane.setMargin(carea, new Insets(5, 0, 0, 0))
      }
    }
  }

  /** Called to check whether we can show this minibuffer.
    * Should throw a feedback exception (with explanation) if showing is not currently allowed. */
  def willShow () :Unit

  /** Called when this minibuffer is made visible. */
  def onShow () :Unit

  /** Called when this minibuffer is cleared. */
  def onClear () :Unit

  override def apply[R] (mode :String, args :Any*) :Future[R] = {
    val result = window.workspace.editor.exec.uiPromise[R]()
    try {
      willShow() // make sure it's OK to activate ourselves

      val buffer = BufferImpl.scratch("*minibuffer*")
      val view = new BufferViewImpl(buffer, 40, 1)
      val modeArgs = ui :: result :: List.copyOf(args)
      val disp = new DispatcherImpl(window, window.resolver(null, buffer), view, ModeLine.Noop,
                                    s"mini-$mode", modeArgs, Nil) {
        override protected def fallbackToText = false
        override protected def createBufferArea () = new BufferArea(view, this) {
          override protected def wasResized (widthChars :Int, heightChars :Int) {
            // only persist width; height is unfortunately delivered bogus values due to JavaFX
            // preferred size retardery
            view.width() = widthChars;
          }
        }
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

  private def formatCompletions (comps :SeqV[String]) :SeqV[String] = {
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
    else comps.grouped(cols).map(_.map(s"%-${colWid}s".format(_)).mkString)
  }
}
