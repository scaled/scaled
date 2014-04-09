//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a configuration var. Modes define configuration vars which can subsequently be
 * customized by the user in a mode configuration file, or interactively.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Var {

    /** A documentary description of this configuration var. This will be shown to the user when
     * they ask to describe the var, so don't hold back on the details. */
    String value ();
}
