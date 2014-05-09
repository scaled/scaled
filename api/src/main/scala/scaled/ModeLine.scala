//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import javafx.scene.control.Tooltip
import reactual.{Value, ValueV}

/** Displays mode summary information to the user. The default information includes:
  * ` - BufferName.ext  L#  [majormode]` and modes can augment the modeline with additional text.
  * This should be used sparingly, as the mode line is not infinite in length.
  */
trait ModeLine {

  /** Adds a label displaying the current value of `value` to the mode line.
    *
    * @param value the reactive value to be displayed.
    * @param tooltip a tooltip to be displayed to the user when they hover over the datum.
    *
    * @return an [[AutoCloseable]] which will remove the datum from the modeline when closed.
    * A mode is advised to `note()` this closeable so that it will automatically be removed if
    * the mode is deactivated. Fire and forget!
    */
  def addDatum (value :ValueV[String], tooltip :String) :AutoCloseable =
    addDatum(value, Value(new Tooltip(tooltip)))

  /** Like [[addDatum(ValueV[String],ValueV[String])]] but which takes a reactive value containing
    * a fully realized tooltip (which can contain an arbitrary scene graph). */
  def addDatum (value :ValueV[String], tooltip :ValueV[Tooltip]) :AutoCloseable
}
