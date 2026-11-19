package gitbucket.core.plugin

import gitbucket.core.model.Profile._
import profile.api._

trait RepositoryHook {
  private val logger = LoggerFactory.getLogger(getClass)

  def created(owner: String, repository: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering created(owner={},repository={})", owner, repository)
      ()
  }
  def deleted(owner: String, repository: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering deleted(owner={},repository={})", owner, repository)
      ()
  }
  def renamed(owner: String, repository: String, newRepository: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering renamed(owner={},repository={},newRepository={})", owner, repository, newRepository)
      ()
  }
  def transferred(owner: String, newOwner: String, repository: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering transferred(owner={},newOwner={},repository={})", owner, newOwner, repository)
      ()
  }
  def forked(owner: String, newOwner: String, repository: String)(implicit session: Session): Unit = {
      logger.error(">>> Entering forked(owner={},newOwner={},repository={})", owner, newOwner, repository)
      ()
  }

}
