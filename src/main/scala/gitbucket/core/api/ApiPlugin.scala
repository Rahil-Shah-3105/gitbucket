package gitbucket.core.api

import gitbucket.core.plugin.PluginInfo

case class ApiPlugin(
  id: String,
  name: String,
  version: String,
  description: String,
  jarFileName: String
)

object ApiPlugin {
  def apply(plugin: PluginInfo): ApiPlugin = {
    private val logger = LoggerFactory.getLogger(getClass)
    logger.debug(">>> Entering apply(plugin={})", plugin)
    ApiPlugin(plugin.pluginId, plugin.pluginName, plugin.pluginVersion, plugin.description, plugin.pluginJar.getName)
  }
}
