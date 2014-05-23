//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Major(name="log", tags=Array("log"), desc="""
       A major mode for displaying log text. Motion commands are available but editing commands
       are not.""")
class LogMode (env :Env) extends ReadingMode(env) {

  // TODO: things?
}
