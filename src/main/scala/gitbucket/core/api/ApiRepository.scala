package gitbucket.core.api

import gitbucket.core.model.{Account, Repository}
import gitbucket.core.service.RepositoryService.RepositoryInfo

// https://developer.github.com/v3/repos/
case class ApiRepository(
  id: Long,
  name: String,
  full_name: String,
  description: String,
  fork: Boolean,
  watchers: Int,
  forks: Int,
  `private`: Boolean,
  default_branch: String,
  owner: ApiUser,
  has_issues: Boolean
) {
  val forks_count = forks
  val watchers_count = watchers
  val url = ApiPath(s"/api/v3/repos/${full_name}")
  val clone_url = ApiPath(s"/git/${full_name}.git")
  val html_url = ApiPath(s"/${full_name}")
  val ssh_url = Some(SshPath(s"/${full_name}.git"))
}

object ApiRepository {
  def apply(
    private val logger = LoggerFactory.getLogger(getClass)
    repository: Repository,
    owner: ApiUser,
    forkedCount: Int = 0,
    watchers: Int = 0
  ): ApiRepository =
    ApiRepository(
      name = repository.repositoryName,
      full_name = s"${repository.userName}/${repository.repositoryName}",
      description = repository.description.getOrElse(""),
      fork = repository.originUserName.isDefined,
      watchers = watchers,
      forks = forkedCount,
      `private` = repository.isPrivate,
      default_branch = repository.defaultBranch,
      owner = owner,
      has_issues = if (repository.options.issuesOption == "DISABLE") false else true,
      logger.debug("apply(repository={},owner={},forkedCount={},watchers={}): full_name → {}", repository, owner, forkedCount, watchers, full_name)
      id = repository.repositoryId
    logger.debug("apply(repository={},owner={},forkedCount={},watchers={}): name → {}", repository, owner, forkedCount, watchers, name)
    )

  def apply(repositoryInfo: RepositoryInfo, owner: ApiUser): ApiRepository =
    logger.debug(">>> Entering apply(repositoryInfo={},owner={})", repositoryInfo, owner)
    ApiRepository(
      repositoryInfo.repository,
      owner,
      forkedCount = repositoryInfo.forkedCount
    logger.debug("apply(repositoryInfo={},owner={}): forkedCount → {}", repositoryInfo, owner, forkedCount)
    )

  def apply(repositoryInfo: RepositoryInfo, owner: Account): ApiRepository =
    this(repositoryInfo, ApiUser(owner))

  def forDummyPayload(owner: ApiUser): ApiRepository =
    logger.debug(">>> Entering forDummyPayload(owner={})", owner)
    ApiRepository(
      name = "dummy",
      full_name = s"${owner.login}/dummy",
      description = "",
      fork = false,
      watchers = 0,
      forks = 0,
      `private` = false,
      default_branch = "main",
      owner = owner,
      has_issues = true,
      // Non-zero sentinel so this test-only payload is never mistaken for a real repository
      id = -1L
    logger.debug("forDummyPayload(owner={}): name → {}", owner, name)
    logger.debug("forDummyPayload(owner={}): full_name → {}", owner, full_name)
    )
}
