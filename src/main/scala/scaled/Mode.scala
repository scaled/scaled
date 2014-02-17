//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled

/** Defines the API shared by major and minor modes. */
trait Mode {

  /** The name of this mode. Displayed to the user. This is generally a simple single word. For
    * example: `scala`, `text`, `commit`. This name string will be prefixed to this mode's fns when
    * called interactively by the user or bound in keymaps, so don't be extravagant. */
  def name :String

  /** Returns the key bindings defined by this mode: a list of `(trigger sequence -> fn binding)`
    * mappings.
    *
    * Trigger sequences are defined thusly: A single key consists of the key identifier (e.g. 'g',
    * 'F1', '-',) prefixed by zero or more modifier keys ('C-' for control, 'M-' for meta, 'A-' for
    * alt, and 'S-' for shift). Key sequences consist of single keys separated by spaces. Examples:
    *  - `e`: lowercase e
    *  - `S-s`: upper case S
    *  - `C-c`: control-c
    *  - `C-c C-i`: control-c followed by control-i
    *
    * The fn bindings defined by the mode, by using an [[Fn]] annotation on a method. The name in
    * the keymap corresponds to the [[Fn.name]] parameter. When a mode refers to its own fns, it
    * may provide just the name, but if a mode (or a mode hook) refers to another mode's fns, it
    * must prefix the name by the name of the mode and a colon (e.g. "scala:goto-term").
    *
    * Key bindings are applied in a stack-like fashion:
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
  def keymap :Seq[(String, String)]

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
