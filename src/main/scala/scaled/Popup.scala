//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Defines a popup, displayed over a buffer. */
case class Popup (text :Seq[String], pos :Popup.Pos, isError :Boolean)
// TODO: allow style runs in the text

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
  case class DnRight (x :Int, y :Int) extends Pos with Dn with Right
  case class UpLeft  (x :Int, y :Int) extends Pos with Up with Left
  case class DnLeft  (x :Int, y :Int) extends Pos with Dn with Left
}
