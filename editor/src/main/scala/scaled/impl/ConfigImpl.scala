//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.util.{HashMap, HashSet}
import scaled._

class ConfigImpl (name :String, defs :List[Config.Defs], parent :Option[ConfigImpl])
    extends Config {

  override def value[T] (key :Config.Key[T]) = resolve(key).value
  override def update[T] (key :Config.Key[T], value :T) = resolve(key).value() = value

  override def toString = s"$name / $parent"

  /** Converts this config to a `properties` file. Config vars that have not been changed from their
    * default values will have commented out entries indicating the default values, and configured
    * entries will appear as normal.
    */
  def toProperties :Seq[String] = {
    val buf = Seq.builder[String]()
    buf += s"# Scaled '$name' config vars"
    buf += s"#"
    buf += s"# This file is managed by Scaled. Uncomment and customize the value of any"
    buf += s"# desired config vars. Other changes will be ignored."
    _vars.values.toSeq.sortBy(_.name) foreach { case cvar :Config.Var[t] =>
      val cval = resolve(cvar.key).asInstanceOf[ConfigValue[t]]
      buf += "" // preceed each var by a blank link and its description
      buf += s"# ${cvar.descrip}"
      val curval = cval.value.get ; val defval = cvar.key.defval(this)
      val (pre, showval) = if (cval.isSet && curval != defval) ("", curval) else ("# ", defval)
      buf += s"$pre${cvar.name}: " + cvar.key.show(showval)
    }
    buf += "" // add a trailing newline
    buf.build()
  }

  /** Used to (re)initialize this config from a file. Call [[apply]] for every key value pair
    * loaded from the file, then call [[complete]]. */
  class Initter (log :Logger) extends ((String, String) => Unit) {
    private val initted = new HashSet[String]()
    def apply (key :String, value :String) = _vars.get(key) match {
      case None => log.log(s"$name config contains unknown/stale setting '$key: $value'.")
      case Some(cvar) => try {
        resolve(cvar.key).init(cvar.key.converter.read(value))
        initted.add(key)
      } catch {
        case e :Exception => log.log(s"$name config contains invalid setting: '$key: $value': $e")
      }
    }
    def complete () {
      // reset any key that was not explicitly initialized from file data
      _vars foreach { (key, cvar) => if (!initted.contains(key)) _vals.get(cvar.key) match {
        case null => // no problem!
        case cval => cval.reset()
      }}
    }
  }

  private def resolve[T] (key :Config.Key[T]) :ConfigValue[T] =
    Mutable.getOrPut(_vals, key, new ConfigValue(key)).asInstanceOf[ConfigValue[T]]

  private class ConfigValue[T] (key :Config.Key[T]) {
    var conn :Connection = _
    val value = Value[T](null.asInstanceOf[T])
    reset()

    def isSet :Boolean = conn == null
    def init (newval :T) {
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
