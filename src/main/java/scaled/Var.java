//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a configuration variable. Modes define configuration variables which can subsequently be
 * customized by the user in a mode configuration file, or interactively.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Var {

    /** A documentary description of this configuration variable. This will be shown to the user
     * when they ask to describe the variable, so don't hold back on the details. */
    String value ();
}
