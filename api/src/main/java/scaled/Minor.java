//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as implementing a minor mode.
 *
 * <p>Note: modes <em>must</em> be implemented by classes named {@code FooMode}. The @Minor
 * annotation is only sought on classes matching that name pattern.</p>
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Minor {

    /**
     * The name of this mode. Displayed to the user. This is generally a simple single lowercase
     * word. Note that modes exist in a global namespace, so pick something unique. Examples:
     * {@code whitespace}, {@code scala}, {@code foo-bar}.
     */
    String name ();

    /**
     * A description of this mode's functionality.
     */
    String desc ();

    /**
     * Zero or more tags describing major modes in which this minor mode should be automatically
     * enabled. See {@link Major#tags} for a list of common tags. For example, a mode which
     * supports editing programming code would include {@code code} here. Omitting this element
     * means that this minor mode will not be automatically enabled and must be manually enabled by
     * the user, or configured by them to be enabled under more specific circumstances.
     */
    String[] tags () default {};
}
