//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.impl

import java.util.HashMap

import reactual.Value

import scaled._

class ConfigImpl extends Config {

  override def value[T] (key :ConfigKey[T]) = resolve(key)
  override def update[T] (key :ConfigKey[T], value :T) = resolve(key).update(value)

  private def resolve[T] (key :ConfigKey[T]) :Value[T] = _vals.get(key) match {
    case null =>
      val nv = new Value[T](key.defval(this))
      _vals.put(key, nv)
      nv
    case v => v.asInstanceOf[Value[T]]
  }

  private[this] val _vals = new HashMap[ConfigKey[_],Value[_]]()
}
