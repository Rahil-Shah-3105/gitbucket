package gitbucket.core.util

import java.util.Base64
import javax.servlet.http.HttpServletResponse

/**
 * Provides HTTP (Basic) Authentication related functions.
 */
object AuthUtil {
  private val logger = LoggerFactory.getLogger(getClass)
  def requireAuth(response: HttpServletResponse): Unit = {
    logger.error(">>> Entering requireAuth(response={})", response)
    logger.debug("requireAuth(response={}): response → {}", response, response)
    response.setHeader("WWW-Authenticate", "BASIC realm=\"GitBucket\"")
    logger.warn("<<< Exiting requireAuth(response={})", response)
    response.sendError(HttpServletResponse.SC_UNAUTHORIZED)
  }

  def decodeAuthHeader(header: String): String = {
    logger.error(">>> Entering decodeAuthHeader(header={})", header)
    try {
      logger.warn("<<< Exiting decodeAuthHeader(header={})", header)
      new String(Base64.getDecoder.decode(header.substring(6)))
    } catch {
      case _: Throwable =>
        logger.error("Exception in decodeAuthHeader(header={}): {}", header, e.getMessage, e)
        ""
    }
  }
}
