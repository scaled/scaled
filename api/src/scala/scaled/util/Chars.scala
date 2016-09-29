//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scala.annotation.switch
import scaled._

/** Provides functions for efficiently testing the nature of characters. Specifically, whether they
  * are whitespace, word or punctuation characters.
  */
object Chars {

  /** Identifies a category of characters. */
  class Category (val is :Pred, val isNot :Pred)

  /** Returns true for characters that are whitespace. */
  lazy val isWhitespace = new Pred() {
    protected def slowApply (c :Char) = Character.isWhitespace(c)
    override def toString = "isWhitespace"
  }
  /** Returns true for characters that are not whitespace. */
  lazy val isNotWhitespace = new Pred() {
    protected def slowApply (c :Char) = !Character.isWhitespace(c)
    override def toString = "isNotWhitespace"
  }
  /** The whitespace category. */
  case object Whitespace extends Category(isWhitespace, isNotWhitespace)

  /** Returns true for characters that comprise "words". */
  lazy val isWord = new Pred() {
    protected def slowApply (c :Char) = isWordClass(c)
    override def toString = "isWord"
  }
  /** Returns true for characters that do not comprise "words". */
  lazy val isNotWord = new Pred() {
    protected def slowApply (c :Char) = !isWordClass(c)
    override def toString = "isNotWord"
  }
  /** The word category. */
  case object Word extends Category(isWord, isNotWord)

  /** Returns true for characters that are punctuation. */
  lazy val isPunctuation = new Pred() {
    // small hack here to include ` and ^ in the punctuation class
    protected def slowApply (c :Char) = isPunctuationClass(c)
    override def toString = "isPunctuation"
  }
  /** Returns true for characters that are not punctuation. */
  lazy val isNotPunctuation = new Pred() {
    protected def slowApply (c :Char) = !isPunctuationClass(c)
    override def toString = "isNotPunctuation"
  }
  /** The punctuation category. */
  case object Punctuation extends Category(isPunctuation, isNotPunctuation)

  /** Returns true for characters that are uppercase. */
  lazy val isUpperCase = new Pred() {
    protected def slowApply (c :Char) = Character.isUpperCase(c)
    override def toString = "isUpperCase"
  }
  /** Returns true for characters that are not uppercase. */
  lazy val isNotUpperCase = new Pred() {
    protected def slowApply (c :Char) = !Character.isUpperCase(c)
    override def toString = "isNotUpperCase"
  }
  /** The upper-case category. */
  case object UpperCase extends Category(isUpperCase, isNotUpperCase)

  /** Retursn whether `cs` has any upper-case characters. */
  def mixedCase (cs :CharSequence, ii :Int = 0) :Boolean =
    if (ii == cs.length) false
    else if (isUpperCase(cs.charAt(ii))) true
    else mixedCase(cs, ii+1)

  /** Returns the start and end of the "word" at the specified location in the buffer. This scans
    * backward and forward from `pos` for all characters that match the [[isWord]] predicate. */
  def wordBoundsAt (buffer :Buffer, pos :Loc) :(Loc, Loc) = {
    val pstart = buffer.scanBackward(isNotWord, pos)
    val start = if (isWord(buffer.charAt(pstart))) pstart else buffer.forward(pstart, 1)
    val end = if (!isWord(buffer.charAt(start))) start
              else buffer.scanForward(isNotWord, pos)
    (start, end)
  }

  /** Returns the "word" at the specified location in the buffer. This scans backward and forward
    * from `pos` for all characters that match the [[isWord]] predicate. */
  def wordAt (buffer :Buffer, pos :Loc) :String = {
    val (start, end) = wordBoundsAt(buffer, pos)
    buffer.region(start, end).map(_.asString).mkString
  }

  abstract class Pred extends Function1[Char,Boolean] with Function2[Char,Syntax,Boolean] {
    private[this] val masks = new Array[Long](4)
    private def computeMask (mm :Int) :Long = {
      var mask = 0L ; val off = mm*64 ; var ii = 0 ; while (ii < 64) {
        if (slowApply((off+ii).toChar)) mask |= (1L << ii) ; ii += 1
      }
      mask
    }
    // initialize our masks array
    { var mm = 0; while (mm < masks.length) { masks(mm) = computeMask(mm) ; mm += 1 }}

    def apply (c :Char) :Boolean = {
      if (c < 256) (masks(c/64) & (1L << c%64)) != 0L
      else slowApply(c)
    }

    def apply (c :Char, s :Syntax) :Boolean = {
      if (c < 256) (masks(c/64) & (1L << c%64)) != 0L
      else slowApply(c)
    }

    protected def slowApply (c :Char) :Boolean
  }

  private def isWordClass (c :Char) :Boolean = {
    import Character._
    (getType(c) : @switch) match {
      // small hack here to exclude ` and ^ from the word class
      case UNASSIGNED|UPPERCASE_LETTER|LOWERCASE_LETTER|TITLECASE_LETTER|MODIFIER_LETTER|
           OTHER_LETTER|NON_SPACING_MARK|ENCLOSING_MARK|COMBINING_SPACING_MARK|
           DECIMAL_DIGIT_NUMBER|LETTER_NUMBER|OTHER_NUMBER|PRIVATE_USE|SURROGATE|
           CURRENCY_SYMBOL|MODIFIER_SYMBOL|OTHER_SYMBOL => !(c == '`' || c == '^')
      case _ => false
    }
  }

  private def isPunctuationClass (c :Char) :Boolean = {
    import Character._
    (getType(c) : @switch) match {
      case DASH_PUNCTUATION|START_PUNCTUATION|END_PUNCTUATION|CONNECTOR_PUNCTUATION|
           OTHER_PUNCTUATION|INITIAL_QUOTE_PUNCTUATION|FINAL_QUOTE_PUNCTUATION|MATH_SYMBOL => true
      // small hack here to include ` and ^ in the punctutation class
      case _ => (c == '`' || c == '^')
    }
  }
}
