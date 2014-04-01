//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Marks a class as implementing a major mode. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Major {

    /** The name of this mode. Displayed to the user. This is generally a simple single lowercase
     * word. Note that modes exist in a global namespace, so pick something unique. Examples:
     * {@code whitespace}, {@code scala}, {@code foo-bar}. */
    String name ();

    /** A description of this mode's functionality. */
    String desc ();
}
