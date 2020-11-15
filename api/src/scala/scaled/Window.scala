//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Represents a top-level editor window. Every editor window contains one or more frames.
  * A window starts with a single frame, consuming all of its space, but those frames can be split
  * horizontally or vertically to introduce more frames into a window.
  */
trait Window {

  /** A single frame in a window. */
  trait Frame {

    /** Returns the current geometry of this frame. */
    def geometry :Geometry

    /** Returns the current size of this frame (in pixels). */
    def size :Size

    /** Returns the window that contains this frame. */
    def window :Window = Window.this

    /** Returns the buffer view that currently occupies this frame. May be `null`. */
    def view :BufferView

    /** Returns the store for the buffer edited previous to the current buffer in this frame.
      * This mainly exists to make it easy to bounce between two buffers in a given frame via
      * `switch-to-buffer`. */
    def prevStore :Option[Store]

    /** Makes the specified buffer the active buffer for this frame.
      * @param focus whether or not to request focus for the window as well.
      * @return the view for the buffer. */
    def visit (buffer :Buffer, focus :Boolean = true) :BufferView

    /** Opens a buffer for `store` and visits it.
      * @return the view for the buffer. */
    def visitFile (store :Store) = visit(window.workspace.openBuffer(store))

    /** Closes and reloads the current buffer, preserving the scroll position and point.
      * Note: any modifications to the current buffer will be lost. Be careful.
      * @return the view for the buffer. */
    def revisitFile () :BufferView = {
      val file = view.buffer.store ; val p = view.point()
      val top = view.scrollTop() ; val left = view.scrollLeft()
      view.buffer.kill()
      val nv = visitFile(file)
      nv.scrollTop() = top
      nv.scrollLeft() = left
      nv.point() = p
      nv
    }
  }

  /** A reactive mapping of window-wide state. */
  val state :RState = new RState()

  /** The active [[Visit.List]] (if any). */
  val visits = Value(new Visit.List("configured visit", Seq()))

  /** A stack of visits made from within this window. */
  val visitStack = new Visit.Stack()

  /** A signal emitted when this window is closed. */
  def onClose :SignalV[Window]

  /** Returns the current geometry of this window. */
  def geometry :Geometry

  /** Returns the current size of this window (in pixels). */
  def size :Size

  /** Returns the of list frames currently in this window. */
  def frames :SeqV[Frame]

  /** Returns the frame that currently has the focus. */
  def focus :Frame

  /** The workspace which owns this window. */
  def workspace :Workspace

  /** Closes this window. When all windows in all workspaces are closed, the process will exit. */
  def close () :Unit

  /** Prompts to save any modified buffers, and closes the window once all are saved or abandoned.
    * May also be aborted by the user. */
  def saveBuffersAndClose () :Unit = {
    val opts = Seq(
      "y"   -> "save the current buffer",
      "n"   -> "skip the current buffer (abandon changes)",
      "q"   -> "skip all remaining buffers",
      "!"   -> "save all remaining buffers",
      "."   -> "save *only* the current buffer, then close",
      "C-g" -> "cancel this close-window command"
      // TODO: C-r to view this buffer?
      // TODO: d to diff this buffer against the file system version
    )
    def saveLoop (dirty :List[Buffer]) :Unit = dirty match {
      case Nil => close()
      case buf :: tail =>
        val prompt = s"${buf.store} is modified. Save?"
        mini.readOpt(prompt, opts) onSuccess(_ match {
          case "y" => buf.save() ; saveLoop(tail)
          case "n" => saveLoop(tail)
          case "q" => saveLoop(Nil)
          case "!" => dirty map(_.save()) ; saveLoop(Nil)
          case "." => buf.save() ; saveLoop(Nil)
        })
    }
    saveLoop(workspace.buffers.filter(_.needsSave).toList)
  }

  /** Returns the subset of the workspace buffers that have ever been visited in this window, in
    * order of most recent activation. */
  def buffers :SeqV[Buffer]

  /** An executor which reports errors via this window. */
  def exec :Executor

  /** Briefly displays a status message to the user in a popup.
    * The status message will also be appended to the `*messages*` buffer. */
  def popStatus (msg :String, subtext :String = "") :Unit

  /** Briefly displays a status message to the user.
    * @param ephemeral if false, the status message will also be appended to the `*messages*`
    * buffer; if true, it disappears forever in a poof of quantum decoherence. */
  def emitStatus (msg :String, ephemeral :Boolean = false) :Unit

  /** Reports an unexpected error to the user.
    * The message will also be appended to the `*messages*` buffer. */
  def emitError (err :Throwable) :Unit = exec.handleError(err)

  /** Clears any lingering status message. A status message usually remains visible until the user
    * types the next key, so this allows any buffer which receives key input to clear the last
    * status message. */
  def clearStatus () :Unit

  /** Provides access to the overlay popup minibuffer. Prefer this for most interactions. */
  def mini :Minibuffer

  /** Provides access to the status-line minibuffer. Use this only when the minibuffer interaction
    * requires the user to see the contents of the main buffer, and hence the popup minibuffer
    * would potentially obscure important data. */
  def statusMini :Minibuffer

  /** Requests that this window be brought to the front of the window stack by the OS. */
  def toFront () :Unit

  /** Returns the (window-scoped) history ring with the specified name. The ring will be created
    * on-demand. */
  def historyRing (name :String) = Mutable.getOrPut(
    Rings(state), name, new Ring(workspace.config(EditorConfig.historySize)) {
      override def toString = s"$name-history"
    })
}

/** Describes the geometry of a [[Window]] or [[Frame]].
  * @param width the width in characters of the referent.
  * @param height the height in characters of the referent.
  * @param x in the case of a window, this is the number of pixels from the left side of the
  * screen at which the window is positioned; in the case of a frame, this is the number of
  * characters from the left side of the containing window at which the frame is positioned.
  * @param y in the case of a window, this is the number of pixels from the top of the screen at
  * which the window is positioned; in the case of a frame, this is the number of characters from
  * the top of the containing window at which the frame is positioned.
  */
case class Geometry (width :Int, height :Int, x :Int, y :Int) {
  override def toString = s"${width}x$height+$x+$y"
}

/** Describes the pixel size of a [[Window]] or [[Frame]]. */
case class Size (width :Int, height :Int) {
  override def toString = s"{width}x{height}"
}

object Geometry {

  /** Creates a [[Geometry]] instance from a descriptor string of the form `WxH+x+y`. */
  def apply (geom :String) :Option[Geometry] = geom match {
    case GeomRe(w, h, x, y) =>
      try Some(apply(w.toInt, h.toInt, x.toInt, y.toInt))
      catch {
        case nfe: NumberFormatException => None
      }
    case _ => None
  }

  private val GeomRe = """(\d+)x(\d+)([+-]\d+)([+-]\d+)""".r
}
