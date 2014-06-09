//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

object TextConfig extends Config.Defs {

  /** The CSS style applied to `header` lines. */
  val headerStyle = "textHeaderFace"
  /** The CSS style applied to `subHeader` lines. */
  val subHeaderStyle = "textSubHeaderFace"
  /** The CSS style applied to `section` lines. */
  val sectionStyle = "textSectionFace"
  /** The CSS style applied to `list` lines. */
  val listStyle = "textListFace"
  /** The CSS style applied to highlighted prefixes. */
  val prefixStyle = "textPrefixFace"
}

@Major(name="text", tags=Array("text"), desc="""
  A major mode for editing plain text. This is used when no more specific mode can be found.
""")
class TextMode (env :Env) extends EditingMode(env) {

  override def configDefs = TextConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/text.css") :: super.stylesheets
}
