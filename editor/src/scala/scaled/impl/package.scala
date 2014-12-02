//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

import java.nio.file.Paths
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

    /** Returns the current working directory of the editor process. */
    def cwd = Paths.get(System.getProperty("user.dir"))

    /** Runs `op` on the main JavaFX thread. */
    def onMainThread (op : =>Unit) = Platform.runLater(new Runnable() {
      def run () = op
    })
  }
}
