//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Major(name="log", tags=Array("log"), desc="""
       A major mode for displaying log text. Motion commands are available but editing commands
       are not.""")
class LogMode (env :Env) extends ReadingMode(env) {

  override def keymap = super.keymap ++ Seq(
    "M-k" -> "clear-log"
  )

  @Fn("Clears the contents of this log buffer.")
  def clearLog () {
    buffer.delete(buffer.start, buffer.end)
  }
}
