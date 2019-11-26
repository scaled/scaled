//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.util.{HashMap, HashSet}
import java.nio.file.{Path, Files}
import scaled._
import scaled.util.{Filler, Properties}

class ConfigImpl (name :String, val file :Path, val scope :Config.Scope,
                  defs :List[Config.Defs], parent :Option[ConfigImpl])
    extends Config {

  override def value[T] (key :Config.Key[T]) = resolve(key).value
  override def update[T] (key :Config.Key[T], value :T) = resolve(key).value() = value

  override def toProperties :Seq[String] = {
    val buf = Seq.builder[String]()
    buf += s"## Scaled '$name' config vars"
    buf += s"##"
    buf += s"## This file is managed by Scaled. Uncomment and customize the value of any"
    buf += s"## desired config vars. Other changes will be ignored."
    _vars.values.toSeq.sortBy(_.name) foreach { case cvar :Config.Var[t] =>
      buf += "" // preceed each var by a blank link and its description
      val filler = new Filler(77)
      filler.append(Filler.flatten(cvar.descrip))
      filler.filled foreach { line => buf += s"## $line" }
      val cval = resolve(cvar.key)
      val curval = cval.value.get ; val defval = cvar.key.defval(this)
      val (pre, showval) = if (cval.isSet && curval != defval) ("", curval) else ("# ", defval)
      buf += s"$pre${cvar.name}: " + cvar.key.show(showval)
    }
    buf += "" // add a trailing newline
    buf.build()
  }

  override def atScope (scope :Config.Scope) = if (this.scope == scope) this else parent match {
    case None => throw new IllegalArgumentException(s"No parent at $scope")
    case Some(p) => p.atScope(scope)
  }

  override def toString = s"$name / $parent"

  /** Reads this config's source file into `this`. Any properties that were set in this instance
    * but were no longer set in the source file are reset to inherited/default. */
  def read (log :Logger) :this.type = {
    read(log, accFn => if (Files.exists(file)) Properties.read(log, file)(accFn))
    this
  }

  // handles the machinery of reading, factored out to enable testing... bleh
  def read (log :Logger, fn :((String,String) => Unit) => Unit) :Unit = {
    val initted = new HashSet[String]()
    fn { (key, value) => _vars.get(key) match {
      case None => log.log(s"$name config contains unknown/stale setting '$key: $value'.")
      case Some(cvar) => try {
        resolve(cvar.key).initFrom(value)
        initted.add(key)
      } catch {
        case e :Exception => log.log(s"$name config contains invalid setting: '$key: $value': $e")
      }
    }}
    // reset any key that was not explicitly initialized from file data
    _vars foreach { (key, cvar) => if (!initted.contains(key)) _vals.get(cvar.key) match {
      case null => // no problem!
      case cval => cval.reset()
    }}
  }

  private def resolve[T] (key :Config.Key[T]) :ConfigValue[T] =
    Mutable.getOrPut(_vals, key, new ConfigValue(key)).asInstanceOf[ConfigValue[T]]

  private class ConfigValue[T] (key :Config.Key[T]) {
    var conn :Connection = _
    val value = Value[T](null.asInstanceOf[T])
    reset()

    def isSet :Boolean = conn == null
    def initFrom (value :String) :Unit = init(key.converter.read(value))
    def init (newval :T) :Unit = {
      value() = newval
      if (conn != null) { conn.close() ; conn = null }
    }
    def reset () :Unit = parent match {
      case None => value.update(key.defval(ConfigImpl.this))
      case Some(p) =>
        if (conn != null) conn.close()
        conn = p.resolve(key).value.onValueNotify(value.update)
    }
  }

  private[this] val _vars = defs.flatMap(_.vars).mapBy(_.name) // String -> Config.Var[_]
  private[this] val _vals = new HashMap[Config.Key[_],ConfigValue[_]]()
}
