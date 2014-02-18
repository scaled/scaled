//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import scaled._

/** Handles the conversion of key presses into execution of the appropriate fns. This includes
  * parsing text trigger sequences into efficient internal structures, mapping trigger sequences to
  * bound fns based on a mode's key bindings, and handling incoming key events and resolving them
  * into trigger sequences.
  *
  * @param mode the major mode which defines our primary key mappings. Minor modes are added (and
  * removed) dynamically, but if our major mode is changed we throw everything out and create a new
  * dispatcher.
  */
class KeyDispatcher (view :BufferView, mode :MajorMode) {

  class Metadata (mode :Mode) {
    val fns = new FnBindings(mode)
  }

  private var _metas = Seq(new Metadata(mode))
}
