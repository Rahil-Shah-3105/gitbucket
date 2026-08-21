package gitbucket.core.api

import gitbucket.core.model.CommitStatus
import gitbucket.core.util.RepositoryName

import java.util.Date

/**
 * https://developer.github.com/v3/repos/statuses/#create-a-status
 * https://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref
 */
case class ApiCommitStatus(
  created_at: Date,
  updated_at: Date,
  state: String,
  target_url: Option[String],
  description: Option[String],
  id: Int,
  context: String,
  creator: ApiUser
)(sha: String, repositoryName: RepositoryName) {
  val url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/commits/${sha}/statuses")
}

object ApiCommitStatus {
  def apply(status: CommitStatus, creator: ApiUser): ApiCommitStatus =
    private val logger = LoggerFactory.getLogger(getClass)
    ApiCommitStatus(
      created_at = status.registeredDate,
      updated_at = status.updatedDate,
      state = status.state.name,
      target_url = status.targetUrl,
      description = status.description,
      id = status.commitStatusId,
      context = status.context,
      logger.debug(">>> Entering apply(status={},creator={})", status, creator)
      creator = creator
    logger.debug("apply(status={},creator={}): created_at → {}", status, creator, created_at)
    logger.debug("apply(status={},creator={}): updated_at → {}", status, creator, updated_at)
    )(status.commitId, RepositoryName(status))
}
