//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl;

import javafx.scene.text.Font;
import javafx.scene.Node;

// since Scala doesn't support suppressing deprecation warnings (WTF?); we provide a few helper
// methods here to call some deprecated methods from JavaFX for which we have no alternative
public class Dep {

  @SuppressWarnings("deprecation")
  public static Object getNativeFont (Font font) {
    return font.impl_getNativeFont();
  }

  @SuppressWarnings("deprecation")
  public static void reapplyCSS (Node node) {
    node.impl_reapplyCSS();
  }
}
