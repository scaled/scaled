//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import javafx.scene.paint.Color

/** Defines the style in which a span of text is rendered. As they are aimed for colorizing code
  * rather than defining arbitrary styled text, faces are limited to foreground and background
  * color, weight, slant, underlining and strikethrough. It is not possible to change the font
  * family or size, those are defined by the user and remain uniform for the entire buffer.
  */
case class Face (foreground :Option[Color] = None,
                 background :Option[Color] = None,
                 bold       :Boolean = false,
                 italic     :Boolean = false,
                 underline  :Boolean = false,
                 strike     :Boolean = false) {
  // TODO: things?
}

/** Defines convenient helper methods for faces. */
object Face {

  /** The default face. This face specifies no colors or other special styles meaning the editor
    * defaults are used. */
  val defaultFace = Face()

  /** Creates a face with the specified foreground color and no other special styles. The color is
    * parsed using [[Color.web]]. */
  def apply (fgColor :String) :Face = Face(foreground=Some(Color.web(fgColor)))
}
