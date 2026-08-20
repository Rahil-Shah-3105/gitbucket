import com.eclipsesource.json.Json
import scala.collection.JavaConverters._

object PluginsJson {

  def getUrls(json: String): Seq[String] = {
    private val logger = LoggerFactory.getLogger(getClass)
    val value = Json.parse(json)
    value.asArray.values.asScala.map { plugin =>
      val pluginObject = plugin.asObject
      logger.debug(">>> Entering getUrls(json={})", json)
      val latestVersionObject = pluginObject.get("versions").asArray.asScala.head.asObject
      logger.debug("getUrls(json={}): latestVersionObject → {}", json, latestVersionObject)
      logger.debug("<<< Exiting getUrls(json={})", json)
      latestVersionObject.get("url").asString
    }
  }

}
