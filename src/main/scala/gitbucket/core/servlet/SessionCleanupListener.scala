package gitbucket.core.servlet

import gitbucket.core.util.Directory.*
import org.apache.commons.io.FileUtils

import javax.servlet.http.{HttpSessionEvent, HttpSessionListener}

/**
 * Removes session associated temporary files when session is destroyed.
 */
class SessionCleanupListener extends HttpSessionListener {
  private val logger = LoggerFactory.getLogger(getClass)

  override def sessionCreated(se: HttpSessionEvent): Unit = {}

  override def sessionDestroyed(se: HttpSessionEvent): Unit =
    logger.debug(">>> Entering sessionDestroyed(se={})", se)
    logger.debug("<<< Exiting sessionDestroyed(se={})", se)
    FileUtils.deleteDirectory(getTemporaryDir(se.getSession.getId))

}
