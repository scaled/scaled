//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Major(name="text", tags=Array("text"), desc="""
  A major mode for editing plain text. This is used when no more specific mode can be found.
""")
class TextMode (env :Env) extends EditingMode(env) {

  override def dispose () {}  // nothing to dispose
}
