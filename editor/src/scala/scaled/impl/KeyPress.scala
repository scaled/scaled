//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import javafx.scene.input.{KeyCode, KeyEvent}
import scaled._

/** Models a key press and any modifier keys that are held down during the press.  */
class KeyPress (
  /** The key identifier (e.g. TAB, a, 7). This is what is used in keymap strings. */
  val id :String,
  /** The text to be inserted in the buffer if this keypress results in `self-insert-command`. */
  val text :String,
  val shift :Boolean, val ctrl :Boolean, val alt :Boolean, val meta :Boolean) {

  /** Returns true if this key press is modified by a function key (ctrl, alt, meta). */
  def isModified :Boolean = ctrl || alt || meta

  /** Whether or not this key press represents a character that the user might actually want inserted
    * into their buffer. */
  def isPrintable :Boolean = !isModified && text != KeyEvent.CHAR_UNDEFINED

  /** Forces `meta` to true if `wantMeta` is true; if `meta` is already true or `wantMeta` is false,
    * returns `this` key press. This is used to 'metafy' the key press following ESC. */
  def metafy (wantMeta :Boolean) :KeyPress =
    if (!wantMeta || meta) this else new KeyPress(id, text, shift, ctrl, alt, true)

  override def toString = {
    import KeyPress._
    val buf = new StringBuilder
    if (shift) buf.append(ShiftId).append("-")
    if (ctrl ) buf.append(CtrlId ).append("-")
    if (alt  ) buf.append(AltId  ).append("-")
    if (meta ) buf.append(MetaId ).append("-")
    buf.append(id).toString
  }

  override def equals (o :Any) = o match {
    case kp :KeyPress =>
      id == kp.id && shift == kp.shift && ctrl == kp.ctrl && alt == kp.alt && meta == kp.meta
    case _ => false
  }

  override def hashCode =
    id.hashCode ^ shift.hashCode ^ ctrl.hashCode ^ alt.hashCode ^ meta.hashCode
}

/** [[KeyPress]] utilities. */
object KeyPress {

  // the identifiers used to denote modifier keys in a key description
  final val ShiftId = "S"
  final val CtrlId  = "C"
  final val AltId   = "A"
  final val MetaId  = "M"
  final val ValidMods = Set(ShiftId, CtrlId, AltId, MetaId)

  /** Creates a key press from a key code and modifier flags. */
  def fromCode (code :KeyCode, shift :Boolean, ctrl :Boolean, alt :Boolean, meta :Boolean) =
    new KeyPress(toKeyId(code), KeyEvent.CHAR_UNDEFINED, shift, ctrl, alt, meta)

  /** Converts a key pressed event into a `KeyPress`. */
  def fromPressed (kev :KeyEvent) :KeyPress = {
    // TEMP: on JavaFX on Linux Alt is Meta and Windows is Alt, but Emacs assigns Alt to Meta and
    // Windows to Super, and Meta is super important so we want to be Emacs-like there
    // TODO: should this be done with a more complex key remapping scheme?
    val isAltDown = if (IsLinux) kev.isMetaDown else kev.isAltDown
    val isMetaDown = if (IsLinux) kev.isAltDown else kev.isMetaDown
    new KeyPress(toKeyId(kev.getCode), kev.getCharacter,
                 kev.isShiftDown, kev.isControlDown, isAltDown, isMetaDown)
  }

  /** Parses a key press sequence (e.g. `C-c C-j`, `a`, `M-.`) into [[KeyPress]]es. If any key
    * press in the sequence is invalid, `Left` will be returned with the text of the invalid key
    * press(es). */
  def toKeyPresses (seq :String) :Either[Seq[String],Seq[KeyPress]] = {
    val errs = Seq.builder[String]() ; val kps = Seq.builder[KeyPress]()
    seq.split(" ").mkSeq foreach { str => toKeyPress(str) match {
      case None     => errs += (if (str == seq) str else s"$str in $seq")
      case Some(kp) => kps  += kp
    }}
    if (!errs.isEmpty) Left(errs.build()) else Right(kps.build())
  }

  /** Converts a single key press description (e.g. 'a', 'C-c', 'M-S-F2', etc.) into a `KeyPress`
    * instance. Invalid descriptions yield `None`. */
  def toKeyPress (desc :String) :Option[KeyPress] = {
    val msb = Set.builder[String]()
    var remain = desc
    while (remain.length > 1 && remain.charAt(1) == '-') {
      msb += remain.substring(0, 1)
      remain = remain.substring(2)
    }
    val modSet = msb.build()
    // validate that there are no spurious modifiers
    if (!(modSet &~ ValidMods).isEmpty) None
    else toKeyCode(remain) map { code =>
      new KeyPress(remain, remain, modSet(ShiftId), modSet(CtrlId), modSet(AltId), modSet(MetaId))
    }
  }

  /** Converts a string into a `KeyCode`. */
  def toKeyCode (key :String) :Option[KeyCode] = IdToCode.get(key)

  /** Converts a `KeyCode` into a `KeyPress.id`. */
  def toKeyId (code :KeyCode) = CodeToId.get(code) || code.name

  private val KeyIds = {
    import KeyCode._
    Seq(
      // printing keys
      SPACE     -> "SPACE",
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
      BRACELEFT     -> "{",
      BRACERIGHT    -> "}",
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
      // TODO: are these right?
      STAR          -> "*",
      POUND         -> "#",

      // the dreaded tab
      TAB -> "TAB",

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
      DELETE    -> "DEL",
      ENTER     -> "ENTER",

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
      BEGIN       -> "BEGIN",
      PAUSE       -> "PAUSE",
      ESCAPE      -> "ESC",
      BACK_SPACE  -> "BS",
      INSERT      -> "INSERT",
      NUM_LOCK    -> "NUMLOCK",
      SCROLL_LOCK -> "SCRLOCK",

      // modifier keys (handled specially)
      SHIFT        -> "<shift>",
      CONTROL      -> "<control>",
      ALT          -> "<alt>",
      META         -> "<meta>",
      COMMAND      -> "<command>",
      CAPS         -> "<capslock>",
      WINDOWS      -> "<windows>",
      CONTEXT_MENU -> "<context>",
      SHORTCUT     -> "<shortcut>",

      // TODO: these look useful
      CUT -> "",
      COPY -> "",
      PASTE -> "",
      UNDO -> "",
      FIND -> "",
      AGAIN -> "",

      // alt-shift-meta-cokebottle (TODO)
      SEPARATOR -> "",
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
      PROPS -> "",
      STOP -> "",
      COMPOSE -> "",
      ALT_GRAPH -> "",
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

      // whatchu talkin' 'bout willis?
      UNDEFINED -> ""
    );
  }

  private val CodeToId = Map[KeyCode,String](KeyIds)
  private val IdToCode = Map[String,KeyCode](KeyIds.collect {
    case (code, str) if (str != "") => (str, code)
  })

  private val IsLinux = System.getProperty("os.name") equalsIgnoreCase "linux"
}
