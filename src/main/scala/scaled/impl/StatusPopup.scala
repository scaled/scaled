//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.animation.FadeTransition
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.control.Label
import javafx.scene.layout.VBox
import javafx.util.Duration

/** Displays status in a popup. */
class StatusPopup extends VBox {

  private val _text = new Label()
  _text.setWrapText(true)
  _text.getStyleClass.add("status")
  private val _subtext = new Label()
  _subtext.setWrapText(true)
  _subtext.getStyleClass.add("substatus")
  private val _fade = new FadeTransition(Duration.millis(1), this)

  getStyleClass.add("overpop")
  getChildren.addAll(_text, _subtext)

  def showStatus (msg :String, subtext :String) {
    toFront()
    _text.setText(msg)
    val haveSubtext = (subtext.length > 0)
    if (haveSubtext) _subtext.setText(subtext)
    _subtext.setVisible(haveSubtext)
    _subtext.setManaged(haveSubtext)
    _fade.stop()
    _fade.setDuration(Duration.millis(150))
    _fade.setFromValue(getOpacity)
    _fade.setToValue(1d)
    _fade.setOnFinished(null)
    _fade.play()
    setVisible(true)
  }

  def clear () {
    if (isVisible) {
      _fade.stop()
      _fade.setDuration(Duration.millis(300))
      _fade.setFromValue(getOpacity)
      _fade.setToValue(0d)
      _fade.play()
      _fade.setOnFinished(new EventHandler[ActionEvent]() {
        override def handle (event :ActionEvent) {
          setVisible(false)
        }
      })
    }
  }
}
