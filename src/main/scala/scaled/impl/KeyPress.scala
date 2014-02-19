//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.input.{KeyCode, KeyEvent}

/** Models a key press and any modifier keys that are held down during the press.  */
case class KeyPress (code :KeyCode, shift :Boolean, ctrl :Boolean, alt :Boolean, meta :Boolean)

/** [[KeyPress]] utilities. */
object KeyPress {

  // the identifiers used to denote modifier keys in a key description
  final val ShiftId = "S"
  final val CtrlId  = "C"
  final val AltId   = "A"
  final val MetaId  = "M"
  final val ValidMods = Set(ShiftId, CtrlId, AltId, MetaId)

  /** Converts a `KeyEvent` to a `KeyPress`. */
  def apply (kev :KeyEvent) :KeyPress = KeyPress(
    kev.getCode, kev.isShiftDown, kev.isControlDown, kev.isAltDown, kev.isMetaDown)

  /** Parses a key press sequence (e.g. `C-c C-j`, `a`, `M-.`) into [[KeyPress]]es. If any key press
    * in the sequence is invalid, `None` will be returned. All invalid key presses will be reported
    * to `onInvalid` (as `kp in seq`) before returning `None`.
    */
  def toKeyPresses (onInvalid :String => Unit, seq :String) :Option[Seq[KeyPress]] = {
    def parse (str :String) = toKeyPress(str) match {
      case None => onInvalid(s"$str in $seq") ; None
      case kp => kp
    }
    val kps = seq.split(" ") map(parse)
    if (kps contains None) None else Some(kps.toSeq flatten)
  }

  /** Converts a single key press description (e.g. 'a', 'C-c', 'M-S-F2', etc.) into a `KeyPress`
    * instance. Invalid descriptions yield `None`.
    */
  def toKeyPress (desc :String) :Option[KeyPress] = {
    val comps = desc.split('-')
    val (mods, key) = comps.splitAt(comps.length-1)
    val modSet = mods.toSet
    // validate that there are no spurious modifiers
    if (!(modSet -- ValidMods).isEmpty) None
    else toKeyCode(key(0)) map { code =>
      KeyPress(code, modSet(ShiftId), modSet(CtrlId), modSet(AltId), modSet(MetaId))
    }
  }

  /** Converts a string into a [[KeyCode]]. */
  def toKeyCode (key :String) :Option[KeyCode] = StringToCode.get(key)

  private val KeyStrings = {
    import KeyCode._
    Seq(
      // printing keys
      SPACE     -> " ",
      COMMA     -> ",",
      MINUS     -> "-",
      PERIOD    -> ".",
      SLASH     -> "/",
      DIGIT0    -> "0",
      DIGIT1    -> "1",
      DIGIT2    -> "2",
      DIGIT3    -> "3",
      DIGIT4    -> "4",
      DIGIT5    -> "5",
      DIGIT6    -> "6",
      DIGIT7    -> "7",
      DIGIT8    -> "8",
      DIGIT9    -> "9",
      SEMICOLON -> ";",
      EQUALS    -> "=",
      A         -> "a",
      B         -> "b",
      C         -> "c",
      D         -> "d",
      E         -> "e",
      F         -> "f",
      G         -> "g",
      H         -> "h",
      I         -> "i",
      J         -> "j",
      K         -> "k",
      L         -> "l",
      M         -> "m",
      N         -> "n",
      O         -> "o",
      P         -> "p",
      Q         -> "q",
      R         -> "r",
      S         -> "s",
      T         -> "t",
      U         -> "u",
      V         -> "v",
      W         -> "w",
      X         -> "x",
      Y         -> "y",
      Z         -> "z",
      OPEN_BRACKET  -> "[",
      BACK_SLASH    -> "\\",
      CLOSE_BRACKET -> "]",
      BACK_QUOTE    -> "`",
      QUOTE         -> "'",
      AMPERSAND     -> "&",
      ASTERISK      -> "*",
      QUOTEDBL      -> "\"",
      LESS          -> "<",
      GREATER       -> ">",
      BRACELEFT     -> "[",
      BRACERIGHT    -> "]",
      AT            -> "@",
      COLON         -> ":",
      CIRCUMFLEX    -> "^",
      DOLLAR        -> "$",
      EURO_SIGN     -> "",
      EXCLAMATION_MARK -> "!",
      INVERTED_EXCLAMATION_MARK -> "",
      LEFT_PARENTHESIS -> "(",
      RIGHT_PARENTHESIS -> ")",
      NUMBER_SIGN   -> "#",
      PLUS          -> "+",
      UNDERSCORE    -> "_",

      // arrow keys
      LEFT  -> "LEFT",
      UP    -> "UP",
      RIGHT -> "RIGHT",
      DOWN  -> "DOWN",

      // function keys
      F1  -> "F1",
      F2  -> "F2",
      F3  -> "F3",
      F4  -> "F4",
      F5  -> "F5",
      F6  -> "F6",
      F7  -> "F7",
      F8  -> "F8",
      F9  -> "F9",
      F10 -> "F10",
      F11 -> "F11",
      F12 -> "F12",
      F13 -> "F13",
      F14 -> "F14",
      F15 -> "F15",
      F16 -> "F16",
      F17 -> "F17",
      F18 -> "F18",
      F19 -> "F19",
      F20 -> "F20",
      F21 -> "F21",
      F22 -> "F22",
      F23 -> "F23",
      F24 -> "F24",

      // number pad keys
      NUMPAD0   -> "NP0",
      NUMPAD1   -> "NP1",
      NUMPAD2   -> "NP2",
      NUMPAD3   -> "NP3",
      NUMPAD4   -> "NP4",
      NUMPAD5   -> "NP5",
      NUMPAD6   -> "NP6",
      NUMPAD7   -> "NP7",
      NUMPAD8   -> "NP8",
      NUMPAD9   -> "NP9",
      MULTIPLY  -> "NP*",
      ADD       -> "NP+",
      SUBTRACT  -> "NP-",
      DECIMAL   -> "NP.",
      DIVIDE    -> "NP/",
      SEPARATOR -> "",
      DELETE    -> "DEL",
      NUM_LOCK  -> "",
      SCROLL_LOCK -> "",
      ENTER     -> "",

      KP_UP    -> "NPUP",
      KP_DOWN  -> "NPDOWN",
      KP_LEFT  -> "NPLEFT",
      KP_RIGHT -> "NPRIGHT",

      // combining keys
      DEAD_GRAVE -> "",
      DEAD_ACUTE -> "",
      DEAD_CIRCUMFLEX -> "",
      DEAD_TILDE -> "",
      DEAD_MACRON -> "",
      DEAD_BREVE -> "",
      DEAD_ABOVEDOT -> "",
      DEAD_DIAERESIS -> "",
      DEAD_ABOVERING -> "",
      DEAD_DOUBLEACUTE -> "",
      DEAD_CARON -> "",
      DEAD_CEDILLA -> "",
      DEAD_OGONEK -> "",
      DEAD_IOTA -> "",
      DEAD_VOICED_SOUND -> "",
      DEAD_SEMIVOICED_SOUND -> "",

      // non-printing keys
      PRINTSCREEN -> "PRINT",
      HELP        -> "HELP",
      PAGE_UP     -> "PGUP",
      PAGE_DOWN   -> "PGDN",
      END         -> "END",
      HOME        -> "HOME",
      TAB         -> "TAB",
      PAUSE       -> "PAUSE",
      ESCAPE      -> "ESC",
      BACK_SPACE  -> "BS",
      INSERT      -> "INS",

      // modifier keys (handled specially)
      META         -> "",
      SHIFT        -> "",
      CONTROL      -> "",
      ALT          -> "",
      CAPS         -> "",
      WINDOWS      -> "",
      CONTEXT_MENU -> "",
      COMMAND      -> "",

      // alt-shift-meta-cokebottle
      CANCEL -> "",
      CLEAR -> "",
      FINAL -> "",
      CONVERT -> "",
      NONCONVERT -> "",
      ACCEPT -> "",
      MODECHANGE -> "",
      ALPHANUMERIC -> "",
      KATAKANA -> "",
      HIRAGANA -> "",
      FULL_WIDTH -> "",
      HALF_WIDTH -> "",
      ROMAN_CHARACTERS -> "",
      ALL_CANDIDATES -> "",
      PREVIOUS_CANDIDATE -> "",
      CODE_INPUT -> "",
      JAPANESE_KATAKANA -> "",
      JAPANESE_HIRAGANA -> "",
      JAPANESE_ROMAN -> "",
      KANA_LOCK -> "",
      INPUT_METHOD_ON_OFF -> "",
      CUT -> "",
      COPY -> "",
      PASTE -> "",
      UNDO -> "",
      AGAIN -> "",
      FIND -> "",
      PROPS -> "",
      STOP -> "",
      COMPOSE -> "",
      ALT_GRAPH -> "",
      BEGIN -> "",
      UNDEFINED -> "",
      SOFTKEY_0 -> "",
      SOFTKEY_1 -> "",
      SOFTKEY_2 -> "",
      SOFTKEY_3 -> "",
      SOFTKEY_4 -> "",
      SOFTKEY_5 -> "",
      SOFTKEY_6 -> "",
      SOFTKEY_7 -> "",
      SOFTKEY_8 -> "",
      SOFTKEY_9 -> "",
      GAME_A -> "",
      GAME_B -> "",
      GAME_C -> "",
      GAME_D -> "",
      STAR -> "",
      POUND -> "",
      POWER -> "",
      INFO -> "",
      COLORED_KEY_0 -> "",
      COLORED_KEY_1 -> "",
      COLORED_KEY_2 -> "",
      COLORED_KEY_3 -> "",
      EJECT_TOGGLE -> "",
      PLAY -> "",
      RECORD -> "",
      FAST_FWD -> "",
      REWIND -> "",
      TRACK_PREV -> "",
      TRACK_NEXT -> "",
      CHANNEL_UP -> "",
      CHANNEL_DOWN -> "",
      VOLUME_UP -> "",
      VOLUME_DOWN -> "",
      MUTE -> "",
      SHORTCUT -> ""
    );
  }

  private val CodeToString = Map[KeyCode,String]() ++ KeyStrings
  private val StringToCode = Map[String,KeyCode]() ++ KeyStrings.collect {
    case (code, str) if (str != "") => (str, code)
  }
}
