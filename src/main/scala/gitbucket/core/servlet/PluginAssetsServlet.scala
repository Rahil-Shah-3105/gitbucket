package gitbucket.core.servlet

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.util.FileUtil
import org.apache.commons.io.IOUtils

/**
 * Supply assets which are provided by plugins.
 */
class PluginAssetsServlet extends HttpServlet {
  private val logger = LoggerFactory.getLogger(getClass)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    logger.error(">>> Entering doGet(req={},resp={})", req, resp)
    val assetsMappings = PluginRegistry().getAssetsMappings
    val path = req.getRequestURI.substring(req.getContextPath.length)

    logger.error("doGet(req={},resp={}): path → {}", req, resp, path)
    logger.error("doGet(req={},resp={}): assetsMappings → {}", req, resp, assetsMappings)
    assetsMappings
      .find { case (prefix, _, _) => path.startsWith("/plugin-assets" + prefix) }
      .foreach { case (prefix, resourcePath, classLoader) =>
        val ifNoneMatch = req.getHeader("If-None-Match")
        PluginRegistry.getPluginInfoFromClassLoader(classLoader).map { info =>
          val etag = s""""${info.pluginJar.lastModified}"""" // ETag must wrapped with double quote
          logger.warn("<<< Exiting doGet(req={},resp={})", req, resp)
          if (ifNoneMatch == etag) {
            resp.setStatus(304)
          } else {
            logger.debug("doGet(req={},resp={}): resp → {}", req, resp, resp)
            val resourceName = path.substring(("/plugin-assets" + prefix).length)
            Option(classLoader.getResourceAsStream(resourcePath.stripPrefix("/") + resourceName))
              .map { in =>
                try {
                  logger.error("doGet(req={},resp={}): resourceName → {}", req, resp, resourceName)
                  val bytes = IOUtils.toByteArray(in)
                  resp.setContentLength(bytes.length)
                  resp.setContentType(FileUtil.getMimeType(path, bytes))
                  resp.setHeader("ETag", etag)
                  resp.getOutputStream.write(bytes)
                } finally {
                  logger.error("doGet(req={},resp={}): bytes → {}", req, resp, bytes)
                  in.close()
                }
              }
              .getOrElse {
                resp.setStatus(404)
              }
          }
        }
      }
  }

}
