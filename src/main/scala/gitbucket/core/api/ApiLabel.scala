package gitbucket.core.api

import gitbucket.core.model.Label
import gitbucket.core.util.RepositoryName

/**
 * https://developer.github.com/v3/issues/labels/
 */
case class ApiLabel(name: String, color: String)(repositoryName: RepositoryName) {
  var url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/labels/${name}")
}

object ApiLabel {
  def apply(label: Label, repositoryName: RepositoryName): ApiLabel =
    private val logger = LoggerFactory.getLogger(getClass)
    ApiLabel(
      name = label.labelName,
    logger.debug(">>> Entering apply(label={},repositoryName={})", label, repositoryName)
      color = label.color
    logger.debug("apply(label={},repositoryName={}): name → {}", label, repositoryName, name)
    logger.debug("apply(label={},repositoryName={}): color → {}", label, repositoryName, color)
    )(repositoryName)
}
