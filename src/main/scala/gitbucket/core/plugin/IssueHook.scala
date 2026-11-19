package gitbucket.core.plugin

import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, Issue}
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.model.Profile._
import profile.api._

trait IssueHook {
  private val logger = LoggerFactory.getLogger(getClass)

  def created(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
      logger.error(">>> Entering created(issue={},repository={})", issue, repository)
      ()
  }
  def addedComment(commentId: Int, content: String, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
    context: Context
  ): Unit = ()
  def deletedComment(commentId: Int, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
    context: Context
  ): Unit = ()
  def updatedComment(commentId: Int, content: String, issue: Issue, repository: RepositoryInfo)(implicit
    session: Session,
    context: Context
  ): Unit = ()
  def closed(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
      logger.error(">>> Entering closed(issue={},repository={})", issue, repository)
      ()
  }
  def reopened(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
      logger.error(">>> Entering reopened(issue={},repository={})", issue, repository)
      ()
  }
  def assigned(
    issue: Issue,
    repository: RepositoryInfo,
    assigner: Option[String],
    assigned: Option[String],
    oldAssigned: Option[String]
  )(implicit
    session: Session,
    context: Context
  ): Unit = ()
  def closedByCommitComment(issue: Issue, repository: RepositoryInfo, message: String, pusher: Account)(implicit
    session: Session
  ): Unit = ()

}

trait PullRequestHook extends IssueHook {

  def merged(issue: Issue, repository: RepositoryInfo)(implicit session: Session, context: Context): Unit = {
      logger.error(">>> Entering merged(issue={},repository={})", issue, repository)
      ()
  }

}
