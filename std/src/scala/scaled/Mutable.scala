//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

import com.google.common.cache.{LoadingCache, CacheBuilder, CacheLoader}
import java.util.{Map => JMap}

/** Utilities for making life easier when mutability is needed. */
object Mutable {

  /** Creates a [[Cache]] configured to fill empty mappings via `filler`. */
  def cacheMap[K,V] (filler :K => V) :LoadingCache[K,V] =
    CacheBuilder.newBuilder().asInstanceOf[CacheBuilder[Any,Any]].build(new CacheLoader[K,V]() {
      override def load (key :K) = filler(key)
    })

  /** Returns the mapping for `key` in `map`, with the caveat that if the mapping is `null`, then
    * `nv` is computed and inserted as a mapping for `key` and also returned. */
  @inline final def getOrPut[K,V] (map :JMap[K,V], key :K, nv : => V) :V = map.get(key) match {
    case null => val v = nv ; map.put(key, v) ; v
    case v    => v
  }
}
