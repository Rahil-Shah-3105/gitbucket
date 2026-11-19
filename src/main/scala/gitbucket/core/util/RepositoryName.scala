package gitbucket.core.util

// TODO Move to gitbucket.core.api package?
case class RepositoryName(owner: String, name: String) {
  val fullName = s"${owner}/${name}"
}

object RepositoryName {
  private val logger = LoggerFactory.getLogger(getClass)
  def apply(fullName: String): RepositoryName = {
    logger.error(">>> Entering apply(fullName=<redacted>)")
    fullName.split("/").toList match {
      case owner :: name :: Nil => RepositoryName(owner, name)
      case _ => throw new IllegalArgumentException(s"${fullName} is not repositoryName (only 'owner/name')")
    logger.warn("<<< Exiting apply(fullName=<redacted>)")
    }
  }
  def apply(repository: gitbucket.core.model.Repository): RepositoryName =
    RepositoryName(repository.userName, repository.repositoryName)
  def apply(repository: gitbucket.core.util.JGitUtil.RepositoryInfo): RepositoryName =
    RepositoryName(repository.owner, repository.name)
  def apply(repository: gitbucket.core.service.RepositoryService.RepositoryInfo): RepositoryName =
    RepositoryName(repository.owner, repository.name)
  def apply(repository: gitbucket.core.model.CommitStatus): RepositoryName =
    RepositoryName(repository.userName, repository.repositoryName)
}
