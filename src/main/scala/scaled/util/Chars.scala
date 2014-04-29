//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.util

import scala.annotation.switch

/** Provides functions for efficiently testing the nature of characters. Specifically, whether they
  * are whitespace, word or punctuation characters.
  */
object Chars {

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

  /** Returns true for characters that are punctuation. */
  lazy val isPunctuation = new Pred() {
    protected def slowApply (c :Char) = isPunctuationClass(c)
    override def toString = "isPunctuation"
  }

  /** Returns true for characters that are not punctuation. */
  lazy val isNotPunctuation = new Pred() {
    protected def slowApply (c :Char) = !isPunctuationClass(c)
    override def toString = "isNotPunctuation"
  }

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

  abstract class Pred extends Function1[Char,Boolean] with Function3[Int,Int,Char,Boolean] {
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

    def apply (row :Int, col :Int, c :Char) :Boolean = {
      if (c < 256) (masks(c/64) & (1L << c%64)) != 0L
      else slowApply(c)
    }

    protected def slowApply (c :Char) :Boolean
  }

  private def isWordClass (c :Char) :Boolean = {
    import Character._
    (getType(c) : @switch) match {
      case UNASSIGNED|UPPERCASE_LETTER|LOWERCASE_LETTER|TITLECASE_LETTER|MODIFIER_LETTER|
           OTHER_LETTER|NON_SPACING_MARK|ENCLOSING_MARK|COMBINING_SPACING_MARK|
           DECIMAL_DIGIT_NUMBER|LETTER_NUMBER|OTHER_NUMBER|PRIVATE_USE|SURROGATE|MATH_SYMBOL|
           CURRENCY_SYMBOL|MODIFIER_SYMBOL|OTHER_SYMBOL => true
      case _ => false
    }
  }

  private def isPunctuationClass (c :Char) :Boolean = {
    import Character._
    (getType(c) : @switch) match {
      case DASH_PUNCTUATION|START_PUNCTUATION|END_PUNCTUATION|CONNECTOR_PUNCTUATION|
           OTHER_PUNCTUATION|INITIAL_QUOTE_PUNCTUATION|FINAL_QUOTE_PUNCTUATION => true
      case _ => false
    }
  }
}
