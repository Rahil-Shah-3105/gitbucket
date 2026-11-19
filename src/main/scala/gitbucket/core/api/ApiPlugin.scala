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
  private val logger = LoggerFactory.getLogger(getClass)
  def apply(plugin: PluginInfo): ApiPlugin = {
    logger.error(">>> Entering apply(plugin={})", plugin)
    logger.warn("<<< Exiting apply(plugin={})", plugin)
    ApiPlugin(plugin.pluginId, plugin.pluginName, plugin.pluginVersion, plugin.description, plugin.pluginJar.getName)
  }
}
