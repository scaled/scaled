//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Provides an interface to the undo-tracking mechanism. */
trait Undoer {

  /** Undoes the most recently accumulated action. The changes made by undoing the action will be
    * accumulated as a redo action. */
  def undo () :Boolean

  /** Redoes the most recently undone action. The changes made by redoing the action will be
    * accumualted as a normal edit (and can thus be re-undone). */
  def redo () :Boolean
}

/** A trait implemented by undoable edits. */
trait Undoable {

  /** Undoes this edit. */
  def undo () :Unit
}
