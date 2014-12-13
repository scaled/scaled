//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

/** Scaled services must extend this class so that they can be notified of lifecycle events. */
abstract class AbstractService {

  /** A callback invoked when a service is first started by Scaled. */
  def didStartup () :Unit

  /** A callback invoked when a service is about to be shutdown by Scaled. */
  def willShutdown () :Unit
}
