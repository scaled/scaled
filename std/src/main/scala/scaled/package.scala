//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

import scala.language.implicitConversions

import java.{util => ju}
import java.util.{function => juf}

package object scaled {

  type uV = scala.annotation.unchecked.uncheckedVariance
  type tailrec = scala.annotation.tailrec

  // for great Java interop
  type JBoolean   = java.lang.Boolean
  type JByte      = java.lang.Byte
  type JShort     = java.lang.Short
  type JCharacter = java.lang.Character
  type JInteger   = java.lang.Integer
  type JLong      = java.lang.Long
  type JFloat     = java.lang.Float
  type JDouble    = java.lang.Double

  type JIterator[+A]  = ju.Iterator[A @uV]
  type JIterable[+A]  = java.lang.Iterable[A @uV]
  type JStringBuilder = java.lang.StringBuilder

  // for great Scala interop
  type SList[+A] = scala.List[A]
  @inline final val SNil = scala.Nil
  type SSeq[+A] = scala.Seq[A]
  type SOption[+A] = scala.Option[A]
  @inline final val SNone = scala.None
  @inline final val SSome = scala.Some

  // some implicit conversions to make working with Java collections easier
  implicit def pimpIterable[A] (iter :JIterable[A]) :Iterable[A] = Iterable.view(iter)
  implicit class IteratorOps[A] (private val ii :ju.Iterator[A]) extends AnyVal {
    def toSeq  = Seq.builder[A]().append(ii).build()
    def toList = List.builder[A]().append(ii).build()
    def toSet  = Set.builder[A]().append(ii).build()
  }
  implicit class ArrayOps[A <: AnyRef] (private val as :Array[A]) extends AnyVal {
    def mkSeq  = Seq.from(as)
    def mkList = List.from(as)
  }
  implicit class CollectionOps[A] (private val cc :ju.Collection[A]) extends AnyVal {
    def toUnordV = Unordered.view(cc)
    def toOrdV   = Ordered.view(cc)
    def toSeq    = Seq.copyOf(cc)
    def toSet    = Set.copyOf(cc)
  }
  implicit class OptionalOps[A] (private val opt :ju.Optional[A]) extends AnyVal {
    def toOpt = Option.from(opt)
  }
  implicit class JListOps[A] (private val list :ju.List[A]) extends AnyVal {
    def toSeqV = Seq.view(list)
    def toSeq  = Seq.copyOf(list)
    def toList = List.copyOf(list)
  }
  implicit class SetOps[A] (private val set :ju.Set[A]) extends AnyVal {
    def toSetV = Set.view(set)
  }
  implicit class MapOps[K,V] (private val map :ju.Map[K,V]) extends AnyVal {
    def toMapV = Map.view(map)
  }

  // some implicit conversions to make working with Scala collections easier
  implicit class SeqOps[A] (private val as :SSeq[A]) extends AnyVal {
    // we can't use toSeq here because that's already defined on scala.Seq &c
    def fromScala = Seq(as :_*)
  }
  implicit class SListOps[A] (private val as :SList[A]) extends AnyVal {
    // we can't use toList here because that's already defined on scala.Seq &c
    def fromScala = List.builder[A]().append(as).build()
  }
  implicit class SOptOps[A] (private val opt :SOption[A]) extends AnyVal {
    def fromScala = Option.from(opt)
  }
}
