//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled.{MajorMode, RBufferView}

/** A major mode for editing plain text. This mode is used when no more specific mode can be found.
  */
class TextMode (view :RBufferView) extends MajorMode {

  override def keymap = Seq() // no custom bindings
  override def dispose () {}  // nothing to dispose
}
