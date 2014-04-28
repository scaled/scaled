//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as implementing a Scaled service.
 *
 * <p>Note: services <em>must</em> be implemented by classes named {@code FooService}. The @Service
 * annotation is only sought on classes matching that name pattern.</p>
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Service {

    /**
     * The name of this service. This is not used for resolution (that's done by type), but it is
     * displayed to the user if they ask for the list of runnign services.
     */
    String name ();

    /**
     * A description of this services's functionality.
     */
    String desc ();

    /**
     * A service can either be a concrete class, which provides the service directly, or an
     * interface which delegates its implementation to another class. In the latter case, this
     * should indicate the name of the implementing class.
     *
     * The name is relative to the package of the service interface, so if a service interface
     * {@code foo.bar.BazService} is implemented by {@code foo.bar.BazServiceImpl} one would
     * specify just {@code BazServiceImpl}. If it were implemented by {@code
     * foo.bar.impl.BazServiceImpl} one would specify {@code impl.BazServiceImpl}.
     */
    String impl () default "";
}
