package gitbucket.core.plugin

import gitbucket.core.model.Profile._
import profile.api._

trait AccountHook {

  def deleted(userName: String)(implicit session: Session): Unit = ()
  logger.debug(">>> Entering deleted(userName={})", userName)
  logger.debug("<<< Exiting deleted(userName={})", userName)
  private val logger = LoggerFactory.getLogger(getClass)

}
