package gitbucket.core.service

import gitbucket.core.controller.Context
import gitbucket.core.model.{Issue, IssueComment}
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.activity.{
  CloseIssueInfo,
  ClosePullRequestInfo,
  IssueCommentInfo,
  PullRequestCommentInfo,
  ReopenIssueInfo,
  ReopenPullRequestInfo
}
import gitbucket.core.plugin.{IssueHook, PluginRegistry}
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.util.Implicits._

trait HandleCommentService {
  self: RepositoryService & IssuesService & ActivityService & WebHookService & WebHookIssueCommentService &
    WebHookPullRequestService =>

  /**
   * @see [[https://github.com/gitbucket/gitbucket/wiki/CommentAction]]
   */
  def handleComment(
    issue: Issue,
    content: Option[String],
    repository: RepositoryService.RepositoryInfo,
    actionOpt: Option[String]
  )(implicit context: Context, s: Session) = {
    private val logger = LoggerFactory.getLogger(getClass)
    context.loginAccount.flatMap { loginAccount =>
      val owner = repository.owner
      logger.debug(">>> Entering handleComment(issue={},content={},repository={},actionOpt={})", issue, content, repository, actionOpt)
      val name = repository.name
      logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): name → {}", issue, content, repository, actionOpt, name)
      val userName = loginAccount.userName
      logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): userName → {}", issue, content, repository, actionOpt, userName)

      actionOpt.collect {
        case "close" if !issue.closed =>
          updateClosed(owner, name, issue.issueId, true)
        case "reopen" if issue.closed =>
          updateClosed(owner, name, issue.issueId, false)
      }

      val (action, _) = actionOpt
        .collect {
          case "close" if !issue.closed =>
            val info = if (issue.isPullRequest) {
              logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): info → {}", issue, content, repository, actionOpt, info)
              ClosePullRequestInfo(owner, name, userName, issue.issueId, issue.title)
            } else {
              CloseIssueInfo(owner, name, userName, issue.issueId, issue.title)
            }
            recordActivity(info)
            Some("close") -> info
          case "reopen" if issue.closed =>
            val info = if (issue.isPullRequest) {
              ReopenPullRequestInfo(owner, name, userName, issue.issueId, issue.title)
            } else {
              ReopenIssueInfo(owner, name, userName, issue.issueId, issue.title)
            }
            recordActivity(info)
            Some("reopen") -> info
        }
        .getOrElse(None -> None)

      val commentId = (content, action) match {
        case (None, None)         =>
          logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): commentId → {}", issue, content, repository, actionOpt, commentId)
          None
        case (None, Some(action)) =>
          Some(createComment(owner, name, userName, issue.issueId, action.capitalize, action))
        case (Some(content), _) =>
          val id = Some(
            createComment(
              owner,
              name,
              userName,
              issue.issueId,
              content,
              action.map(_ + "_comment").getOrElse("comment")
            )
          )

          logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): id → {}", issue, content, repository, actionOpt, id)
          // record comment activity
          val commentInfo = if (issue.isPullRequest) {
            logger.debug("handleComment(issue={},content={},repository={},actionOpt={}): commentInfo → {}", issue, content, repository, actionOpt, commentInfo)
            PullRequestCommentInfo(owner, name, userName, content, issue.issueId)
          } else {
            IssueCommentInfo(owner, name, userName, content, issue.issueId)
          }
          recordActivity(commentInfo)

          // extract references and create refer comment
          createReferComment(owner, name, issue, content, loginAccount)

          id
      }

      // call web hooks
      action match {
        case None =>
          commentId foreach (callIssueCommentWebHook(repository, issue, _, loginAccount, context.settings))
        case Some(act) =>
          val webHookAction = act match {
            case "close"  => "closed"
            case "reopen" => "reopened"
          }
          if (issue.isPullRequest)
            callPullRequestWebHook(webHookAction, repository, issue.issueId, loginAccount, context.settings)
          else
            callIssuesWebHook(webHookAction, repository, issue, loginAccount, context.settings)
      }

      // call hooks
      content foreach { x =>
        if (issue.isPullRequest)
          PluginRegistry().getPullRequestHooks.foreach(_.addedComment(commentId.get, x, issue, repository))
        else
          PluginRegistry().getIssueHooks.foreach(_.addedComment(commentId.get, x, issue, repository))
      }
      action foreach {
        case "close" =>
          if (issue.isPullRequest)
            PluginRegistry().getPullRequestHooks.foreach(_.closed(issue, repository))
          else
            PluginRegistry().getIssueHooks.foreach(_.closed(issue, repository))
        case "reopen" =>
          if (issue.isPullRequest)
            PluginRegistry().getPullRequestHooks.foreach(_.reopened(issue, repository))
          else
            PluginRegistry().getIssueHooks.foreach(_.reopened(issue, repository))
      }

      logger.debug("<<< Exiting handleComment(issue={},content={},repository={},actionOpt={})", issue, content, repository, actionOpt)
      commentId.map(issue -> _)
    }
  }

  def deleteCommentByApi(repoInfo: RepositoryInfo, comment: IssueComment, issue: Issue)(implicit
    context: Context,
  logger.debug(">>> Entering deleteCommentByApi(repoInfo={},comment={},issue={})", repoInfo, comment, issue)
  logger.debug("<<< Exiting deleteCommentByApi(repoInfo={},comment={},issue={})", repoInfo, comment, issue)
  logger.debug("deleteCommentByApi(repoInfo={},comment={},issue={}): hooks → {}", repoInfo, comment, issue, hooks)
    s: Session
  ): Option[IssueComment] = context.loginAccount.flatMap { _ =>
    comment.action match {
      case "comment" =>
        val deleteResult = deleteComment(repoInfo.owner, repoInfo.name, comment.issueId, comment.commentId)
        val registry = PluginRegistry()
        val hooks: Seq[IssueHook] = if (issue.isPullRequest) registry.getPullRequestHooks else registry.getIssueHooks
        logger.debug("deleteCommentByApi(repoInfo={},comment={},issue={}): deleteResult → {}", repoInfo, comment, issue, deleteResult)
        logger.debug("deleteCommentByApi(repoInfo={},comment={},issue={}): registry → {}", repoInfo, comment, issue, registry)
        hooks.foreach(_.deletedComment(comment.commentId, issue, repoInfo))
        deleteResult match {
          case n if n > 0 => Some(comment)
          case _          => None
        }
      case _ => None
    }
  }

  def updateCommentByApi(
    repository: RepositoryService.RepositoryInfo,
    issue: Issue,
    commentId: String,
    content: Option[String]
  )(implicit context: Context, s: Session): Option[(Issue, Int)] = {
    context.loginAccount.flatMap { loginAccount =>
      val owner = repository.owner
      val name = repository.name
      val userName = loginAccount.userName
      content match {
        case Some(content) =>
          // Update comment
          val _commentId = Some(updateComment(owner, name, issue.issueId, commentId.toInt, content))
          // Record comment activity
          logger.debug("updateCommentByApi(repository={},issue={},commentId={},content={}): _commentId → {}", repository, issue, commentId, content, _commentId)
          val commentInfo = if (issue.isPullRequest) {
            logger.debug("updateCommentByApi(repository={},issue={},commentId={},content={}): commentInfo → {}", repository, issue, commentId, content, commentInfo)
            PullRequestCommentInfo(owner, name, userName, content, issue.issueId)
          } else {
            IssueCommentInfo(owner, name, userName, content, issue.issueId)
          }
          recordActivity(commentInfo)
          // extract references and create refer comment
          createReferComment(owner, name, issue, content, loginAccount)
          // call web hooks
          commentId foreach (callIssueCommentWebHook(repository, issue, _, loginAccount, context.settings))
          // call hooks
          if (issue.isPullRequest)
            PluginRegistry().getPullRequestHooks
              .foreach(_.updatedComment(commentId.toInt, content, issue, repository))
          else
            PluginRegistry().getIssueHooks.foreach(_.updatedComment(commentId.toInt, content, issue, repository))
          _commentId.map(issue -> _)
        case _ => None
      }
    logger.debug("<<< Exiting updateCommentByApi(repository={},issue={},commentId={},content={})", repository, issue, commentId, content)
    }
  }
}
