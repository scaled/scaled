//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._

@Major(name="help", tags=Array("help"), desc="""
  A major mode for displaying help text. Motion commands are available, editing commands are not.
""")
class HelpMode (env :Env) extends ReadOnlyTextMode(env) {

  override def keymap = super.keymap.
    bind("visit", "ENTER");
  // TODO: other things?

  private val noopVisit = Visit.Tag(new Visit() {
    override protected def go (window :Window) {}
  })

  @Fn("Visits the target of the current line, if any.")
  def visit () {
    buffer.line(view.point()).lineTag(noopVisit)(window)
  }
}
