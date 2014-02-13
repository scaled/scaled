//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

import javafx.scene.input.KeyEvent

/** Encapsulates an action on a buffer view, which will be triggered by a series of one or more
  * keystrokes. This is the "forgetful" version, which doesn't care about the keystrokes that
  * triggered the action. See [TriggerAction] for actions that care about which keys caused them to
  * be triggered. */
trait Action {
  /** Performs the action.
    * @param view the view in which the action was triggered. */
  def act (view :BufferView) :Unit
}

/** Encapsulates an action on a buffer view, which will be triggered by a series of one or more
  * keystrokes. This is the "I care about which keys triggered me" version. See also [Action] which
  * is for actions that don't care how they were triggered. */
trait TriggerAction {
  /** Performs the action.
    * @param view the view in which the action was triggered.
    * @param trigger the sequence of one or more key presses that triggered this action. */
  def act (view :BufferView, trigger :Seq[KeyEvent]) :Unit
}
