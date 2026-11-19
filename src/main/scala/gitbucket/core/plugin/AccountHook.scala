package gitbucket.core.plugin

import gitbucket.core.model.Profile._
import profile.api._

trait AccountHook {
  private val logger = LoggerFactory.getLogger(getClass)

  def deleted(userName: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering deleted(userName={})", userName)
      logger.warn("<<< Exiting deleted(userName={})", userName)
      ()
  }

}
