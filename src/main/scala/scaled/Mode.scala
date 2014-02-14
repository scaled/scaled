//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Defines the API shared by major and minor modes. */
trait Mode {

  /** Returns the key bindings defined by this mode. Key bindings are applied in a stack-like
    * fashion:
    *  - start with global key bindings
    *  - push the major mode key bindings
    *  - push customizations to that mode's key bindings specified by the user
    *  - push minor mode key bindings
    *  - push customizations to that mode's key bindings specified by the user
    *  - and so forth for all active minor modes
    *  - finally push global key binding customizations specified by the user.
    *
    * When a key is pressed, the bindings on the top of the stack are searched first (user defined
    * global key bindings), then on down the stack until a match is found, or we fall off the
    * bottom after searching the stock global key bindings.
    */
  def keymap :Seq[KeyBinding]

  /** Cleans up any external resources managed by this mode. This is called when the mode is disabled
    * or the buffer containing the mode is going away. */
  def dispose () :Unit
}

// /** [Mode] related types and helpers. */
// object Mode {

//   /** Defines a hook that can be used to customize a mode's keymap, initialization or cleanup. */
//   class Hook[M <: Mode] {

//     def onInit (mode :M) {}

//   }
// }

/** Provides the foundation for a major editing mode. A major editing mode customizes the behavior
  * of the editor, usually while editing a certain type of file (a `.java` source file, for
  * example).
  *
  * The mode instance generally exists for the lifetime of a buffer, and can wire up reactions to
  * changes in the buffer or editor in addition to making simpler behavior changes like modifying
  * the keymap.
  */
abstract class MajorMode extends Mode {

}

/** Provides the foundation for a minor editing mode. A minor editing mode customizes the behavior
  * of the editor in a way that augments the behavior of one or more major editing modes (for
  * example, by checking the spelling of all words in the buffer and binding a face to those that
  * are misspelled).
  */
abstract class MinorMode extends Mode {

}
