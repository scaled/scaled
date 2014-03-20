//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

import java.io.File

import javafx.beans.value.ObservableValue
import javafx.beans.value.ChangeListener

package scaled {
  package object impl {

    type JBoolean = java.lang.Boolean

    /** Wraps `fn` in a [[ChangeListener]]. */
    def onChangeB (fn :Boolean => Unit) = new ChangeListener[JBoolean]() {
      override def changed (prop :ObservableValue[_ <: JBoolean], oldV :JBoolean, newV :JBoolean) =
        fn(newV.booleanValue)
    }

    /** Wraps `fn` in a [[ChangeListener]]. */
    def onChange[T] (fn :T => Unit) = new ChangeListener[T]() {
      override def changed (prop :ObservableValue[_ <: T], oldV :T, newV :T) = fn(newV)
    }

    /** Returns the current working directory of the editor process. */
    def cwd () = new File(System.getProperty("user.dir"))
  }
}
