//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.major

import scaled._

/** A base class for modes that want to use {@code TextMode} styles, but which are read-only. This
  * extends [[ReadingMode]] rather than [[EditingMode]]. */
abstract class ReadOnlyTextMode (env :Env) extends ReadingMode(env) {

  override def stylesheets = stylesheetURL("/text.css") :: super.stylesheets
}
