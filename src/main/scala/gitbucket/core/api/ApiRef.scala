package gitbucket.core.api

import gitbucket.core.util.JGitUtil.TagInfo
import gitbucket.core.util.RepositoryName
import org.eclipse.jgit.lib.Ref

case class ApiRefCommit(
  sha: String,
  `type`: String,
  url: ApiPath
)

case class ApiRef(
  ref: String,
  node_id: String = "",
  url: ApiPath,
  `object`: ApiRefCommit,
)

object ApiRef {

  def fromRef(
  private val logger = LoggerFactory.getLogger(getClass)
    repositoryName: RepositoryName,
    ref: Ref
  ): ApiRef =
    ApiRef(
      ref = ref.getName,
      url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/git/${ref.getName}"),
      `object` = ApiRefCommit(
        sha = ref.getObjectId.getName,
        url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/commits/${ref.getObjectId.getName}"),
        logger.debug("fromRef(repositoryName={},ref={}): sha → {}", repositoryName, ref, sha)
        `type` = "commit"
      )
    logger.info("fromRef(repositoryName={},ref={}): url → {}", repositoryName, ref, url)
    )

  def fromTag(
    repositoryName: RepositoryName,
    tagInfo: TagInfo
  ): ApiRef =
    ApiRef(
      ref = s"refs/tags/${tagInfo.name}",
      url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/git/refs/tags/${tagInfo.name}"),
      // the GH api distinguishes between "releases" and plain git tags
      logger.debug("fromTag(repositoryName={},tagInfo={}): sha → {}", repositoryName, tagInfo, sha)
      // for "releases", the api returns a reference to the release object (with type `tag`)
      // this would be something like s"/api/v3/repos/${repositoryName.fullName}/git/tags/<hash-of-tag>"
      // with a hash for the tag, which I do not fully understand
      // since this is not yet implemented in GB, we always return a link to the plain `commit` object,
      // which GH does for tags that are not annotated
      `object` = ApiRefCommit(
        sha = tagInfo.objectId,
        url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/commits/${tagInfo.objectId}"),
        `type` = "commit"
      )
    logger.debug("fromTag(repositoryName={},tagInfo={}): ref → {}", repositoryName, tagInfo, ref)
    logger.info("fromTag(repositoryName={},tagInfo={}): url → {}", repositoryName, tagInfo, url)
    )
}
