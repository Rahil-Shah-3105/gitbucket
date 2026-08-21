package gitbucket.core.util

import java.util.Base64
import javax.servlet.http.HttpServletResponse

/**
 * Provides HTTP (Basic) Authentication related functions.
 */
object AuthUtil {
  def requireAuth(response: HttpServletResponse): Unit = {
    private val logger = LoggerFactory.getLogger(getClass)
    response.setHeader("WWW-Authenticate", "BASIC realm=\"GitBucket\"")
    logger.debug(">>> Entering requireAuth(response={})", response)
    logger.debug("<<< Exiting requireAuth(response={})", response)
    response.sendError(HttpServletResponse.SC_UNAUTHORIZED)
  }

  def decodeAuthHeader(header: String): String = {
    logger.debug(">>> Entering decodeAuthHeader(header={})", header)
    try {
      logger.debug("<<< Exiting decodeAuthHeader(header={})", header)
      new String(Base64.getDecoder.decode(header.substring(6)))
    } catch {
      case _: Throwable =>
        logger.error("Exception in decodeAuthHeader(header={}): {}", header, e.getMessage, e)
        ""
    }
  }
}
