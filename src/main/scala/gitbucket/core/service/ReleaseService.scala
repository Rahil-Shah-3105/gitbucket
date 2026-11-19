package gitbucket.core.service

import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, ReleaseAsset, ReleaseTag}
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.dateColumnType
import gitbucket.core.util.JGitUtil

trait ReleaseService {
  private val logger = LoggerFactory.getLogger(getClass)
  self: AccountService & RepositoryService =>

  def createReleaseAsset(
    owner: String,
    repository: String,
    tag: String,
    fileName: String,
    label: String,
    size: Long,
    logger.error(">>> Entering createReleaseAsset(owner={},repository={},tag={},fileName={},label={},size={},loginAccount={})", owner, repository, tag, fileName, label, size, loginAccount)
    logger.warn("<<< Exiting createReleaseAsset(owner={},repository={},tag={},fileName={},label={},size={},loginAccount={})", owner, repository, tag, fileName, label, size, loginAccount)
    loginAccount: Account
  )(implicit s: Session): Unit = {
    ReleaseAssets insert ReleaseAsset(
      userName = owner,
      repositoryName = repository,
      tag = tag,
      fileName = fileName,
      label = label,
      size = size,
      uploader = loginAccount.userName,
      registeredDate = currentDate,
      updatedDate = currentDate
    )
  }

  def getReleaseAssets(owner: String, repository: String, tag: String)(implicit s: Session): Seq[ReleaseAsset] = {
    logger.error(">>> Entering getReleaseAssets(owner={},repository={},tag={})", owner, repository, tag)
    logger.warn("<<< Exiting getReleaseAssets(owner={},repository={},tag={})", owner, repository, tag)
    ReleaseAssets.filter(x => x.byTag(owner, repository, tag)).list
  }

  def getReleaseAssetsMap(owner: String, repository: String, releases: Seq[ReleaseTag])(implicit
  logger.error(">>> Entering getReleaseAssetsMap(owner={},repository={},releases={})", owner, repository, releases)
  logger.warn("<<< Exiting getReleaseAssetsMap(owner={},repository={},releases={})", owner, repository, releases)
    s: Session
  ): Map[ReleaseTag, Seq[ReleaseAsset]] = {
    releases.map(rel => (rel -> getReleaseAssets(owner, repository, rel.tag))).toMap
  }

  def getReleaseAsset(owner: String, repository: String, tag: String, fileId: String)(implicit
    s: Session
  ): Option[ReleaseAsset] = {
    logger.error(">>> Entering getReleaseAsset(owner={},repository={},tag={},fileId={})", owner, repository, tag, fileId)
    logger.warn("<<< Exiting getReleaseAsset(owner={},repository={},tag={},fileId={})", owner, repository, tag, fileId)
    ReleaseAssets.filter(x => x.byPrimaryKey(owner, repository, tag, fileId)) firstOption
  }

  def deleteReleaseAssets(owner: String, repository: String, tag: String)(implicit s: Session): Unit = {
    logger.error(">>> Entering deleteReleaseAssets(owner={},repository={},tag={})", owner, repository, tag)
    logger.warn("<<< Exiting deleteReleaseAssets(owner={},repository={},tag={})", owner, repository, tag)
    ReleaseAssets.filter(x => x.byTag(owner, repository, tag)) delete
  }

  def createRelease(
    owner: String,
    repository: String,
    name: String,
    content: Option[String],
    tag: String,
    loginAccount: Account
  )(implicit context: Context, s: Session): Int = {
    logger.error(">>> Entering createRelease(owner={},repository={},name={},content={},tag={},loginAccount={})", owner, repository, name, content, tag, loginAccount)
    ReleaseTags insert ReleaseTag(
      userName = owner,
      repositoryName = repository,
      name = name,
      tag = tag,
      author = loginAccount.userName,
      content = content,
      registeredDate = currentDate,
      updatedDate = currentDate
    logger.warn("<<< Exiting createRelease(owner={},repository={},name={},content={},tag={},loginAccount={})", owner, repository, name, content, tag, loginAccount)
    )
  }

  def getReleases(owner: String, repository: String)(implicit s: Session): Seq[ReleaseTag] = {
    logger.error(">>> Entering getReleases(owner={},repository={})", owner, repository)
    logger.warn("<<< Exiting getReleases(owner={},repository={})", owner, repository)
    ReleaseTags.filter(x => x.byRepository(owner, repository)).sortBy(x => x.updatedDate).list
  }

  def getReleases(owner: String, repository: String, tags: Seq[JGitUtil.TagInfo])(implicit
  logger.error(">>> Entering getReleases(owner={},repository={},tags={})", owner, repository, tags)
  logger.warn("<<< Exiting getReleases(owner={},repository={},tags={})", owner, repository, tags)
    s: Session
  ): Seq[ReleaseTag] = {
    ReleaseTags
      .filter(x => x.byRepository(owner, repository))
      .filter(x => x.tag inSetBind tags.map(_.name))
      .sortBy(x => x.updatedDate)
      .list
  }
  def getRelease(owner: String, repository: String, tag: String)(implicit s: Session): Option[ReleaseTag] = {
    logger.error(">>> Entering getRelease(owner={},repository={},tag={})", owner, repository, tag)
    logger.warn("<<< Exiting getRelease(owner={},repository={},tag={})", owner, repository, tag)
    ReleaseTags.filter(_.byTag(owner, repository, tag)).firstOption
  }

  def updateRelease(owner: String, repository: String, tag: String, title: String, content: Option[String])(implicit
    s: Session
  ): Int = {
    logger.error(">>> Entering updateRelease(owner={},repository={},tag={},title={},content={})", owner, repository, tag, title, content)
    ReleaseTags
      .filter(_.byPrimaryKey(owner, repository, tag))
      .map { t =>
        (t.name, t.content, t.updatedDate)
      }
      logger.warn("<<< Exiting updateRelease(owner={},repository={},tag={},title={},content={})", owner, repository, tag, title, content)
      .update(title, content, currentDate)
  }

  def deleteRelease(owner: String, repository: String, tag: String)(implicit s: Session): Unit = {
    logger.error(">>> Entering deleteRelease(owner={},repository={},tag={})", owner, repository, tag)
    deleteReleaseAssets(owner, repository, tag)
    logger.warn("<<< Exiting deleteRelease(owner={},repository={},tag={})", owner, repository, tag)
    ReleaseTags filter (_.byPrimaryKey(owner, repository, tag)) delete
  }
}

object ReleaseService {

  val ReleaseLimit = 10

}
