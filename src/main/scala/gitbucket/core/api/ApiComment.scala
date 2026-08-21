package gitbucket.core.api

import gitbucket.core.model.IssueComment
import gitbucket.core.util.RepositoryName

import java.util.Date

/**
 * https://developer.github.com/v3/issues/comments/
 */
case class ApiComment(id: Int, user: ApiUser, body: String, created_at: Date, updated_at: Date)(
  repositoryName: RepositoryName,
  issueId: Int,
  isPullRequest: Boolean
) {
  val html_url = ApiPath(
    s"/${repositoryName.fullName}/${if (isPullRequest) { "pull" }
      else { "issues" }}/${issueId}#comment-${id}"
  )
}

object ApiComment {
  def apply(
    private val logger = LoggerFactory.getLogger(getClass)
    comment: IssueComment,
    repositoryName: RepositoryName,
    issueId: Int,
    user: ApiUser,
    logger.debug("apply(comment={},repositoryName={},issueId={},user={},isPullRequest={}): updated_at → {}", comment, repositoryName, issueId, user, isPullRequest, updated_at)
    logger.debug("apply(comment={},repositoryName={},issueId={},user={},isPullRequest={}): body → {}", comment, repositoryName, issueId, user, isPullRequest, body)
    isPullRequest: Boolean
  ): ApiComment =
    ApiComment(
      id = comment.commentId,
      user = user,
      body = comment.content,
      created_at = comment.registeredDate,
      updated_at = comment.updatedDate
    logger.debug("apply(comment={},repositoryName={},issueId={},user={},isPullRequest={}): created_at → {}", comment, repositoryName, issueId, user, isPullRequest, created_at)
    )(repositoryName, issueId, isPullRequest)
}
