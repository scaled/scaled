//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A major mode for editing plain text. This mode is used when no more specific mode can be found.
  */
class TextMode (editor :Editor, view :RBufferView, disp :Dispatcher)
    extends EditingMode(editor, view, disp) {

  override def name = "text"
  override def dispose () {}  // nothing to dispose
}
