package gitbucket.core.util

// TODO Move to gitbucket.core.api package?
case class RepositoryName(owner: String, name: String) {
  val fullName = s"${owner}/${name}"
}

object RepositoryName {
  def apply(fullName: String): RepositoryName = {
    private val logger = LoggerFactory.getLogger(getClass)
    fullName.split("/").toList match {
      logger.debug(">>> Entering apply(fullName={})", fullName)
      case owner :: name :: Nil => RepositoryName(owner, name)
      case _ => throw new IllegalArgumentException(s"${fullName} is not repositoryName (only 'owner/name')")
    }
  }
  def apply(repository: gitbucket.core.model.Repository): RepositoryName =
    RepositoryName(repository.userName, repository.repositoryName)
  def apply(repository: gitbucket.core.util.JGitUtil.RepositoryInfo): RepositoryName =
    logger.debug(">>> Entering apply(repository={})", repository)
    RepositoryName(repository.owner, repository.name)
  def apply(repository: gitbucket.core.service.RepositoryService.RepositoryInfo): RepositoryName =
    RepositoryName(repository.owner, repository.name)
  def apply(repository: gitbucket.core.model.CommitStatus): RepositoryName =
    RepositoryName(repository.userName, repository.repositoryName)
}
