//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.HashMap
import reactual.Value
import scala.collection.mutable.ArrayBuffer
import scaled._
import spray.json._

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
      lookup(v.key) match {
        case null => buf += s"# ${v.name}: ${v.key.toString(v.key.defval(this))}"
        case   rv => // TODO: ugh
          val curval = v.key.asInstanceOf[Config.Key[Object]].toString(rv.get.asInstanceOf[Object])
          buf += s"${v.name}: $curval"
      }
    }
    buf += "" // add a trailing newline
    buf
  }

  // TODO: route this into *Messages* buffer or something
  protected def warn (msg :String) = println(msg)

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

  private def put (key :String, value :String) :Unit = _vars.get(key) match {
    case None => warn(s"$name config contains unknown/stale setting '$key: $value'.")
    case Some(cvar) => try {
      resolve(cvar.key)() = cvar.key.converter.fromString(value)
    } catch {
      case e :Exception => warn(s"$name config contains invalid setting: '$key: $value': $e")
    }
  }

  private[this] val _vars = Map[String,Config.Var[_]]() ++ defs.flatMap(_.vars).map(v => v.name -> v)
  private[this] val _vals = new HashMap[Config.Key[_],Value[_]]()
}

object ConfigImpl {
  import scala.collection.convert.WrapAsScala._

  /** Reads the config information from `file` and updates `into` with the configuration read from
    * the file. */
  def readInto (file :File, into :ConfigImpl) :Unit =
    readInto(file.getName, Files.readAllLines(file.toPath), into)

  /** Parses the config information in `text` and updates `into` with the parsed values. */
  def readInto (name :String, lines :Seq[String], into :ConfigImpl) {
    def isComment (l :String) = (l startsWith "#") || (l.length == 0)
    lines map(_.trim) filterNot(isComment) foreach { _ split(":", 2) match {
      case Array(key, value) => into.put(key.trim, value.trim)
      // TODO: require errFn so that we can report this bogosity somewhere sensible
      case other => println(s"$name contains invalid config line:\n${other.toSeq}")
    }}
  }
}
