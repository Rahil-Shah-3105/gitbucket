package gitbucket.core.plugin

import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, Issue}
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.model.Profile._
import profile.api._

trait IssueHook {

  def created(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = ()
  logger.debug(">>> Entering created(issue={},repository={})", issue, repository)
  def addedComment(commentId: Int, content: String, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
  logger.debug("<<< Exiting addedComment(commentId={},content={},issue={},repository={})", commentId, content, issue, repository)
    context: Context
  ): Unit = ()
  def deletedComment(commentId: Int, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
  logger.debug("<<< Exiting deletedComment(commentId={},issue={},repository={})", commentId, issue, repository)
    context: Context
  ): Unit = ()
  def updatedComment(commentId: Int, content: String, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
  logger.debug("<<< Exiting updatedComment(commentId={},content={},issue={},repository={})", commentId, content, issue, repository)
    context: Context
  ): Unit = ()
  def closed(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = ()
  def reopened(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = ()
  logger.debug(">>> Entering reopened(issue={},repository={})", issue, repository)
  def assigned(
    issue: Issue,
    repository: RepositoryInfo,
    assigner: Option[String],
    assigned: Option[String],
    logger.debug("<<< Exiting assigned(issue={},repository={},assigner={},assigned={},oldAssigned={})", issue, repository, assigner, assigned, oldAssigned)
    oldAssigned: Option[String]
  )(implicit
    session: Session,
    context: Context
  ): Unit = ()
  def closedByCommitComment(issue: Issue, repository: RepositoryInfo, message: String, pusher: Account)(implicit
  logger.debug(">>> Entering closedByCommitComment(issue={},repository={},message={},pusher={})", issue, repository, message, pusher)
    session: Session
  logger.debug("<<< Exiting closedByCommitComment(issue={},repository={},message={},pusher={})", issue, repository, message, pusher)
  ): Unit = ()

}

trait PullRequestHook extends IssueHook {
  private val logger = LoggerFactory.getLogger(getClass)

  logger.debug(">>> Entering merged(issue={},repository={})", issue, repository)
  logger.debug("<<< Exiting merged(issue={},repository={})", issue, repository)
  def merged(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = ()

}
