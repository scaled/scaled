//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as implementing a Scaled service plugin. Service plugins are arbitrary classes
 * (presumably implementing a common interface) that allow a service to be extended by other
 * packages. Plugins are tagged with a unique tag and a service can then request a list of all
 * plugins with a particular tag, and do what it will with them.
 *
 * <p>Note: plugins <em>must</em> be implemented by classes named {@code FooPlugin}. The @Plugin
 * annotation is only sought on classes matching that name pattern.</p>
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Plugin {

    /**
     * The tag that identifies the plugin being implemented by this class.
     */
    String tag ();
}
