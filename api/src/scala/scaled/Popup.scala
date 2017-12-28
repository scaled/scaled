//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Defines a popup, displayed over a buffer. */
case class Popup (
  /** The buffer to be displayed by this popup. */
  buffer :Buffer,
  /** Controls the position of the popup and its extent. */
  pos :Popup.Pos,
  /** Indicates that this popup is ephemeral, which means it is automatically cleared the next time
    * the user types a key, just like status messages. Persistent (non-ephemeral) popups must be
    * cleared manually. */
  isEphemeral :Boolean,
  /** Indicates that this popup is displaying an error. Error popups are styled in a more attention
    * getting manner than non-error (informatinonal) popups. */
  isError :Boolean) {

  /** Returns a copy of this popup that is persistent, not ephemeral. */
  def toPersistent :Popup = copy(isEphemeral=false)

  /** Returns a copy of this popup as an error popup. */
  def toError :Popup = copy(isError=true)
}

/** [[Popup]] types and whatnot. */
object Popup {

  /** Creates a popup with `text` as its contents. */
  def text (text :Ordered[String], pos :Popup.Pos) :Popup = lines(text map Line.apply, pos)

  /** Creates a popup with `lines` as its contents. */
  def lines (lines :Ordered[LineV], pos :Popup.Pos) :Popup = {
    assert(!lines.isEmpty, "Popup must contain at least one line.")
    val buffer = Buffer.scratch("*popup*")
    buffer.insert(Loc.Zero, lines)
    apply(buffer, pos, true, false)
  }

  /** Creates a popup with `buffer` as its contents. */
  def buffer (buffer :Buffer, pos :Popup.Pos) :Popup = apply(buffer, pos, true, false)

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
