//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.util.HashMap
import reactual.Value
import scala.collection.mutable.ArrayBuffer
import scaled._

class ConfigImpl (name :String, defs :List[Config.Defs], parent :Option[ConfigImpl]) extends Config {

  override def value[T] (key :Config.Key[T]) = resolve(key)
  override def update[T] (key :Config.Key[T], value :T) = resolve(key).update(value)

  override def toString = s"$name / $parent"

  /** Converts this config to a `properties` file. Config vars that have not been changed from their
    * default values will have commented out entries indicating the default values, and configured
    * entries will appear as normal.
    */
  def toProperties :Seq[String] = {
    val buf = new ArrayBuffer[String]()
    buf += s"# Scaled '$name' config vars"
    buf += s"#"
    buf += s"# This file is managed by Scaled. Uncomment and customize the value of any"
    buf += s"# desired config vars. Other changes will be ignored."
    _vars.values.toSeq.sortBy(_.name) foreach { v =>
      buf += "" // preceed each var by a blank link and its description
      buf += s"# ${v.descrip}"
      val defval = v.key.show(v.key.defval(this))
      val rv = lookup(v.key)
      // TODO: oh type system, you fail me so
      val curval = if (rv == null) defval
                   else v.key.asInstanceOf[Config.Key[Object]].show(rv.get.asInstanceOf[Object])
      val pre = if (curval == defval) "# " else ""
      buf += s"$pre${v.name}: $curval"
    }
    buf += "" // add a trailing newline
    buf
  }

  /** Returns an init put function used to populate this instance from a file. */
  def init (log :Logger) :(String, String) => Unit = (key, value) => _vars.get(key) match {
    case None => log.log(s"$name config contains unknown/stale setting '$key: $value'.")
    case Some(cvar) => try {
      resolve(cvar.key)() = cvar.key.converter.read(value)
    } catch {
      case e :Exception => log.log(s"$name config contains invalid setting: '$key: $value': $e")
    }
  }

  private def resolve[T] (key :Config.Key[T]) :Value[T] = lookup(key) match {
    case null =>
      if (parent.isEmpty == key.global) {
        val nv = new Value[T](key.defval(this))
        _vals.put(key, nv)
        nv
      } else parent.map(_.resolve(key)).getOrElse {
        throw new IllegalStateException(s"Global config asked to resolve local key: $key")
      }
    case v => v
  }

  private def lookup[T] (key :Config.Key[T]) = _vals.get(key).asInstanceOf[Value[T]]

  private[this] val _vars = Map[String,Config.Var[_]]() ++ defs.flatMap(_.vars).map(v => v.name -> v)
  private[this] val _vals = new HashMap[Config.Key[_],Value[_]]()
}
