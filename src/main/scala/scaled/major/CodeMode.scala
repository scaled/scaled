//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A base class for major modes which edit program code.
  */
abstract class CodeMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends EditingMode(editor, config, view, disp) {

}
