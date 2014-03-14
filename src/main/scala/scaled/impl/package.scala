//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

import java.io.File

import javafx.application.Platform
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

    /** Invokes `op` on the next UI tick. */
    def defer (op : =>Unit) :Unit = Platform.runLater(new Runnable() {
      override def run () = op
    })

    /** Returns the current working directory of the editor process. */
    def cwd () = new File(System.getProperty("user.dir"))
  }
}
