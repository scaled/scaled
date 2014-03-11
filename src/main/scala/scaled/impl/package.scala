//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

import javafx.beans.value.ObservableValue
import javafx.beans.value.ChangeListener

package scaled {
  package object impl {

    type JBoolean = java.lang.Boolean

    def onChangeB (fn :Boolean => Unit) = new ChangeListener[JBoolean]() {
      override def changed (prop :ObservableValue[_ <: JBoolean], oldV :JBoolean, newV :JBoolean) =
        fn(newV.booleanValue)
    }

    def onChange[T] (fn :T => Unit) = new ChangeListener[T]() {
      override def changed (prop :ObservableValue[_ <: T], oldV :T, newV :T) = fn(newV)
    }
  }
}
