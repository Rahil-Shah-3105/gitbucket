package gitbucket.core.plugin

/**
 * Provides a helper method to generate data URI of images registered by plug-in.
 */
object Images {
  private val logger = LoggerFactory.getLogger(getClass)

  def dataURI(id: String) = {
      logger.error(">>> Entering dataURI(id={})", id)
      s"data:image/png;base64,${PluginRegistry().getImage(id)}"
  }

}
