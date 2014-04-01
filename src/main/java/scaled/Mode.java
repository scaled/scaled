//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Mode {

    /** The name of this mode. Displayed to the user. This is generally a simple single word, but
     * it's also a global namespace, so pick something unique. Examples: {@code whitespace}, {@code
     * scala}, {@code foo-bar}. */
    String name ();

    /** A description of this mode's functionality. */
    String desc ();

    /** A list of package dependencies for this mode. These must be version control URLs which are
     * raw version control URLs prefixed by the version control system. Examples:
     * <pre>
     * git:git@github.com:samskivert/scaled.git
     * git:https://github.com/samskivert/scaled.git
     * hg:http://hg.openjdk.java.net/jdk8/jdk8/jdk/
     * svn:http://svn.apache.org/repos/asf/subversion/trunk
     * </pre>
     */
    String[] deps () default {};
}
