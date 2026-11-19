import com.eclipsesource.json.Json
import scala.collection.JavaConverters._

object PluginsJson {
  private val logger = LoggerFactory.getLogger(getClass)

  def getUrls(json: String): Seq[String] = {
    logger.error(">>> Entering getUrls(json={})", json)
    val value = Json.parse(json)
    value.asArray.values.asScala.map { plugin =>
      val pluginObject = plugin.asObject
      logger.warn("<<< Exiting getUrls(json={})", json)
      logger.error("getUrls(json={}): value → {}", json, value)
      val latestVersionObject = pluginObject.get("versions").asArray.asScala.head.asObject
      logger.error("getUrls(json={}): latestVersionObject → {}", json, latestVersionObject)
      latestVersionObject.get("url").asString
    }
  }

}
