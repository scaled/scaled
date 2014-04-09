//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import com.google.common.io.Files
import java.nio.charset.StandardCharsets
import java.io.File
import java.util.HashMap
import reactual.Value
import scaled._
import spray.json._

class ConfigImpl (name :String, defs :List[Config.Defs], parent :Option[ConfigImpl]) extends Config {

  override def value[T] (key :Config.Key[T]) = resolve(key)
  override def update[T] (key :Config.Key[T], value :T) = resolve(key).update(value)

  override def toString = s"$name / $parent"

  /** Generates JSON from `cfg`, with "field.default" entries for config vars that have not been
    * changed from their default values. */
  def toJson :String = {
    import DefaultJsonProtocol._
    (_vars.values.toSeq.sortBy(_.name) map { v =>
      lookup(v.key) match {
        case null => (s"${v.name}.default", v.key.toString(v.key.defval(this)))
        case   rv => // TODO: ugh
          (s"${v.name}", v.key.asInstanceOf[Config.Key[Object]].toString(
            rv.get.asInstanceOf[Object]))
      }
    }).toMap.toJson.prettyPrint
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

  private def apply (cfg :Map[String,JsValue]) = cfg foreach {
    case (nm, jsval) =>
      _vars.get(nm) match {
        case None => if (!(nm endsWith ".default")) warn(
          s"$name config contains unknown/stale setting '$nm'.")
        case Some(cvar) =>
          jsval match {
            case jsstr :JsString => try {
              resolve(cvar.key)() = cvar.key.converter.fromString(jsstr.value)
            } catch {
              case e :Exception => warn(
                s"$name config contains invalid setting [key=$nm, value=${jsstr.value}]: $e")
            }
            case _ => println(s"TODO: ConfigImpl.put($jsval)")
          }
      }
  }

  private[this] val _vars = Map[String,Config.Var[_]]() ++ defs.flatMap(_.vars).map(v => v.name -> v)
  private[this] val _vals = new HashMap[Config.Key[_],Value[_]]()
}

object ConfigImpl {
  import DefaultJsonProtocol._

  /** Reads the config information from `file` and updates `into` with the configuration read from
    * the file. */
  def readInto (file :File, into :ConfigImpl) :Unit =
    readInto(file.getName, Files.toString(file, StandardCharsets.UTF_8), into)

  /** Parses the config information in `text` and updates `into` with the parsed values. */
  def readInto (name :String, text :String, into :ConfigImpl) {
    JsonParser(text) match {
      case json :JsObject => into.apply(json.fields)
      // TODO: require errFn so that we can report this bogosity somewhere sensible
      case what => println(s"$name contains invalid JSON:\n$text")
    }
  }
}
