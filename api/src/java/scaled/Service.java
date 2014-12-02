//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

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
     * <p>The name is relative to the package of the service interface, so if a service interface
     * {@code foo.bar.BazService} is implemented by {@code foo.bar.BazServiceImpl} one would specify
     * just {@code BazServiceImpl}. If it were implemented by {@code foo.bar.impl.BazServiceImpl}
     * one would specify {@code impl.BazServiceImpl}.</p>
     */
    String impl () default "";

    /**
     * Indicates that this service should be resolved immediately at editor startup. NOTE: don't use
     * this unless absolutely necessary. This slows down editor startup, so you had better be pretty
     * confident that your service is needed immediately rather than on demand. Further, your
     * service should do as little as possible upon resolution, and defer everything it can until it
     * is actually used by a mode or other service.
     *
     * <p>An example use case is the project service, which has to inject itself into the buffer
     * loading process so that it can populate a config root before any modes are resolved on the
     * buffer, enabling per-project configuration. The project service does nothing at resolution
     * time other than to register a hook to be called when a new workspace is created, and that
     * hook registers a per-workspace hook which does project resolution when a buffer is
     * created.</p>
     */
    boolean autoLoad () default false;
}
