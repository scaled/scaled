//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Mode(name="text", desc="""
      A major mode for editing plain text. This is used when no more specific mode can be found.""")
class TextMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends EditingMode(editor, config, view, disp) {

  override def dispose () {}  // nothing to dispose
}
