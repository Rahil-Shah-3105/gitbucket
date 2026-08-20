package gitbucket.core.plugin

import gitbucket.core.model.Profile._
import profile.api._

trait RepositoryHook {

  def created(owner: String, repository: String)(implicit session: Session): Unit = ()
  private val logger = LoggerFactory.getLogger(getClass)
  def deleted(owner: String, repository: String)(implicit session: Session): Unit = ()
  logger.debug(">>> Entering created(owner={},repository={})", owner, repository)
  def renamed(owner: String, repository: String, newRepository: String)(implicit session: Session): Unit = ()
  logger.debug(">>> Entering deleted(owner={},repository={})", owner, repository)
  def transferred(owner: String, newOwner: String, repository: String)(implicit session: Session): Unit = ()
  logger.debug(">>> Entering renamed(owner={},repository={},newRepository={})", owner, repository, newRepository)
  logger.debug(">>> Entering forked(owner={},newOwner={},repository={})", owner, newOwner, repository)
  logger.debug(">>> Entering transferred(owner={},newOwner={},repository={})", owner, newOwner, repository)
  logger.debug("<<< Exiting forked(owner={},newOwner={},repository={})", owner, newOwner, repository)
  def forked(owner: String, newOwner: String, repository: String)(implicit session: Session): Unit = ()

}
