//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

@Service(name="config", impl="impl.ConfigManager",
         desc="Handles the resolution of configuration data.")
trait ConfigService {
  import Config._

  /** Resolves the configuration for `name`-mode at the specified `scope`.
    * @param defs the config defs stack for the mode in question, which is used if the
    * configuration is being resolved for the first time.
    */
  def resolveModeConfig (scope :Scope, name :String, defs :List[Defs]) :Config

  /** Returns `(name, config)` for all services with a registered config. */
  def serviceConfigs :Seq[(String,Config)]

  /** Resolves the configuration for the `name` service. Service configuration only exists at the
    * global scope, so no scope qualifier is provided.
    * @param defs the config defs stack (usually only a single config object), which is used if
    * the configuration is being resolved for the first time.
    */
  def resolveServiceConfig (name :String, defs :List[Defs]) :Config
}
