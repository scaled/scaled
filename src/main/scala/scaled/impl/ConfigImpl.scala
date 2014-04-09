//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.util.HashMap

import reactual.Value

import scaled._

class ConfigImpl (name :String, parent :Option[ConfigImpl]) extends Config {

  override def value[T] (key :Config.Key[T]) = resolve(key)
  override def update[T] (key :Config.Key[T], value :T) = resolve(key).update(value)

  override def toString = s"$name / $parent"

  private def resolve[T] (key :Config.Key[T]) :Value[T] = _vals.get(key) match {
    case null =>
      if (parent.isEmpty == key.global) {
        val nv = new Value[T](key.defval(this))
        _vals.put(key, nv)
        nv
      } else parent.map(_.resolve(key)).getOrElse {
        throw new IllegalStateException(s"Global config asked to resolve local key: $key")
      }
    case v => v.asInstanceOf[Value[T]]
  }

  private[this] val _vals = new HashMap[Config.Key[_],Value[_]]()
}
