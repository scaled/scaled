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
     * enabled. See {@link Major#tags} for a list of common tags. For example, a mode which supports
     * editing programming code would include {@code code} here. A minor mode can also use the tag
     * {@code "*"} to indicate that it is always applicable. Use sparingly!
     */
    String[] tags () default {};

    /**
     * Zero or more types of buffer state objects which must be present on a buffer for this mode to
     * be enabled. If any of these types are missing from the state, the mode will not be enabled.
     * For example, {@code SubProcessMode} is automatically activated when a {@code SubProcess}
     * instance is added to buffer state.
     *
     * <p>Note: a mode will not be automatically enabled based on state types alone, the minor mode
     * must also match at least one major mode tag. If a minor mode really should be enabled in all
     * cases where its state is satisfied, use {@code "*"} as the mode's tags.
     */
    Class<?>[] stateTypes () default {};
}
