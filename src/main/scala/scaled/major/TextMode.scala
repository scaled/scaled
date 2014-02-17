//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

/** A major mode for editing plain text. This mode is used when no more specific mode can be found.
  */
class TextMode extends EditingMode {

  override def name = "text"
  override def dispose () {}  // nothing to dispose
}
