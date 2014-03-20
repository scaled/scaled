//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scala.annotation.switch
import scala.collection.mutable.{HashMap => MHashMap}

/** A marker trait for syntax classes. */
trait Syntax
// TODO: should we seal this trait? is it useful to allow syntax class extensions?

/** Defines standard syntax classes. */
object Syntax {
  // TODO: which of these do we really need?
  case object Whitespace  extends Syntax
  case object Word        extends Syntax
  case object Symbol      extends Syntax
  case object Punctuation extends Syntax
  case object Quote       extends Syntax
  case object Escape      extends Syntax
  case class  OpenDelim (close :Char) extends Syntax
  case class  CloseDelim (open :Char) extends Syntax
}

/** Maps characters to a syntax class. Editing modes make use of syntax tables for a variety of
  * purposes, including word motion (they need to know which characters are word characters),
  * intelligently handling escaped characters, and more.
  *
  * The default syntax table uses [[Character.getType]] to classify characters into [[Word]],
  * [[Punctuation]], [[OpenDelim]], [[CloseDelim]] and [[Whitespace]] classes. Coding modes may
  * further refine these classifications based on the language they support.
  */
class SyntaxTable {
  import Syntax._

  /** Returns the syntax class for `c`. */
  def apply (c :Char) :Syntax = {
    if (c < _ascii.length) _ascii(c)
    else _resolved(c) match {
      case null => _resolved.put(c, resolve(c)) ; _resolved(c)
      case    s => s
    }
  }

  /** Resolves the syntax class for `c`. The first time a character's syntax is requested, this
    * (potentially slow) method is called and the resulting class is cached for future lookups. */
  protected def resolve (c :Char) :Syntax = {
    import Character._
    (getType(c) : @switch) match {
      case UNASSIGNED                => Word
      case UPPERCASE_LETTER          => Word
      case LOWERCASE_LETTER          => Word
      case TITLECASE_LETTER          => Word
      case MODIFIER_LETTER           => Word
      case OTHER_LETTER              => Word
      case NON_SPACING_MARK          => Word // TODO: something else? I think these generally
      case ENCLOSING_MARK            => Word // show up in the middle of words, so we probably
      case COMBINING_SPACING_MARK    => Word // don't want them to cause word breaks
      case DECIMAL_DIGIT_NUMBER      => Word
      case LETTER_NUMBER             => Word
      case OTHER_NUMBER              => Word
      case SPACE_SEPARATOR           => Whitespace
      case LINE_SEPARATOR            => Whitespace
      case PARAGRAPH_SEPARATOR       => Whitespace
      case CONTROL                   => Whitespace // TODO: Punctuation?
      case FORMAT                    => Whitespace // TODO: Punctuation?
      case PRIVATE_USE               => Word
      case SURROGATE                 => Word
      case DASH_PUNCTUATION          => Punctuation
      case START_PUNCTUATION         => Punctuation
      case END_PUNCTUATION           => Punctuation
      case CONNECTOR_PUNCTUATION     => Punctuation
      case OTHER_PUNCTUATION         => Punctuation
      case MATH_SYMBOL               => Word // TODO: Punctuation?
      case CURRENCY_SYMBOL           => Word // TODO: Punctuation?
      case MODIFIER_SYMBOL           => Word // TODO: Punctuation?
      case OTHER_SYMBOL              => Word // TODO: Punctuation?
      case INITIAL_QUOTE_PUNCTUATION => Punctuation
      case FINAL_QUOTE_PUNCTUATION   => Punctuation
      case _                         => Word
    }
  }

  // precompute syntax for the ascii characters so we can look them up without boxing
  private val _ascii = new Array[Syntax](256)
  for (ii <- 0 until _ascii.length) _ascii(ii) = resolve(ii.toChar)

  // any remaining mappings are cached in this map (TODO: use a specialized CharMap, we eventually
  // want a zero boxing solution)
  private val _resolved = new MHashMap[Char,Syntax]() {
    override def default (key :Char) = null
  }
}
