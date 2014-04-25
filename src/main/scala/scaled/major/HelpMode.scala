//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Major(name="help", tags=Array("help"), desc="""
  A major mode for displaying help text. Motion commands are available but editing commands are not.
""")
class HelpMode (env :Env) extends ReadingMode(env) {

  override def dispose () {}  // nothing to dispose
}
