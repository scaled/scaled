//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines an fn binding. An fn binding is a function that is associated with a name and
 * descriptive documentation, which can be invoked interactively by the user either by being bound
 * to a key sequence, or directly by name.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Fn {

    /** A documentary description of the effects of this fn. This will be shown to the user when
     * they ask to describe the fn, so don't hold back on the details. */
    String value ();
}
