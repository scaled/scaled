//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman

import scala.collection.mutable.{ArrayBuffer, Map => MMap}

class Config (lines :Iterable[String]) {
  import Config._

  private val _errors = ArrayBuffer[String]()
  private val _data = MMap[String,ArrayBuffer[String]]()
  lines.map(trim).filter(_.length > 0).flatMap(split) foreach {
    case (key, value) => _data.getOrElseUpdate(key.trim, ArrayBuffer()) += value.trim
  }

  def apply[T] (key :String, parser :Parser[T]) :T = {
    val lines = _data.remove(key)
    var res = parser.zero
    val errors = ArrayBuffer[String]()
    lines foreach { lines =>
      for (n <- lines) try {
        val nval = parser.parse(n)
        res = Some(if (res.isDefined) parser.accum(key)(res.get, nval) else nval)
      } catch {
        case e :Exception => errors += e.getMessage
      }
      res
    }
    _errors ++= errors
    res.getOrElse(throw new IllegalArgumentException(
      s"Missing or invalid binding for '$key' [data=$lines, errors=${box(errors)}]"))
  }

  def finish () :Seq[String] = {
    _data foreach { case (k, d) => _errors += s"Unknown binding: '$k' ${box(d)}" }
    _errors
  }

  private def split (line :String) = line.split(":", 2) match {
    case Array(key, value) => Some(key -> value)
    case _ => _errors += s"Invalid: $line" ; None
  }

  private def box (strs :Seq[String]) = strs.mkString("[", ", ", "]")

  private def trim (line :String) = line.indexOf('#') match {
    case -1 => line.trim
    case ii => line.substring(0, ii).trim
  }
}

/** Defines a simple parsing and accumulation scheme for config files. */
object Config {

  // defines simply parsing and accumulation scheme for package config files
  abstract class Parser[T] {
    def parse (text :String) :T
    def zero :Option[T] = None
    // by default we disallow accumulation; a key can only be bound once
    def accum (key :String)(oval :T, nval :T) :T =
      throw new Exception(s"'$key' already defined:\n  old: '$oval'\n  new: '$nval'")
  }

  val StringP = new Parser[String]() {
    def parse (text :String) = text
  }

  val DependP = new Parser[List[Depend]]() {
    def parse (text :String) = Depend.parse(text) :: Nil
    override def zero = Some(List())
    override def accum (key :String)(have :List[Depend], next :List[Depend]) = have ++ next
  }

  val SourceP = new Parser[Source]() {
    def parse (text :String) = Source.parse(text)
  }
}
