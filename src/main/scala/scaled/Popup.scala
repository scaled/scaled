//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Defines a popup, displayed over a buffer. */
case class Popup (
  /** The text that will be displayed in the popup. */
  text :Seq[String], // TODO: take Seq[LineV] and support styled text
  /** Controls the position of the popup and its extent. */
  pos :Popup.Pos,
  /** Indicates that this popup is ephemeral, which means it is automatically cleared the next time
    * the user types a key, just like status messages. Persistent (non-ephemeral) popups must be
    * cleared manually. */
  isEphemeral :Boolean = true,
  /** Indicates that this popup is displaying an error. Error popups are styled in a more attention
    * getting manner than non-error (informatinonal) popups. */
  isError :Boolean = false)

object Popup {
  /** Defines the position and orientation of a popup. */
  sealed trait Pos {
    val x :Int
    val y :Int

    def vx (x :Double, width :Double, ileft :Double, iright: Double): Double
    def vy (y :Double, height :Double, lh :Double): Double
  }

  trait Left {
    def vx (x :Double, width :Double, ileft :Double, iright: Double) = x - width + iright
  }
  trait Right {
    def vx (x :Double, width :Double, ileft :Double, iright: Double) = x - ileft
  }
  trait Up {
    def vy (y :Double, height :Double, lh :Double) = y - height
  }
  trait Dn {
    def vy (y :Double, height :Double, lh :Double) = y + lh
  }

  case class UpRight (x :Int, y :Int) extends Pos with Up with Right
  object UpRight {
    def apply (pos :Loc) :UpRight = apply(pos.col, pos.row)
  }
  case class DnRight (x :Int, y :Int) extends Pos with Dn with Right
  object DnRight {
    def apply (pos :Loc) :DnRight = apply(pos.col, pos.row)
  }
  case class UpLeft  (x :Int, y :Int) extends Pos with Up with Left
  object UpLeft {
    def apply (pos :Loc) :UpLeft = apply(pos.col, pos.row)
  }
  case class DnLeft  (x :Int, y :Int) extends Pos with Dn with Left
  object DnLeft {
    def apply (pos :Loc) :DnLeft = apply(pos.col, pos.row)
  }
}
