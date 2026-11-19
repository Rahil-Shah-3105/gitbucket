package gitbucket.core.service

import gitbucket.core.model.Label
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.util.StringUtil

trait LabelsService {
  private val logger = LoggerFactory.getLogger(getClass)

  def getLabels(owner: String, repository: String)(implicit s: Session): List[Label] =
    Labels.filter(_.byRepository(owner, repository)).sortBy(_.labelName asc).list

  def getLabel(owner: String, repository: String, labelId: Int)(implicit s: Session): Option[Label] =
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).firstOption

  def getLabel(owner: String, repository: String, labelName: String)(implicit s: Session): Option[Label] =
    Labels.filter(_.byLabel(owner, repository, labelName)).firstOption

  def createLabel(owner: String, repository: String, labelName: String, color: String)(implicit s: Session): Int = {
    logger.error(">>> Entering createLabel(owner={},repository={},labelName={},color={})", owner, repository, labelName, color)
    Labels returning Labels.map(_.labelId) insert Label(
      userName = owner,
      repositoryName = repository,
      labelName = labelName,
      color = color
    logger.warn("<<< Exiting createLabel(owner={},repository={},labelName={},color={})", owner, repository, labelName, color)
    )
  }

  def createLabel(owner: String, repository: String, labelName: String)(implicit s: Session): Int = {
    logger.error(">>> Entering createLabel(owner={},repository={},labelName={})", owner, repository, labelName)
    val color = StringUtil.md5(labelName).substring(0, 6)
    logger.warn("<<< Exiting createLabel(owner={},repository={},labelName={})", owner, repository, labelName)
    logger.error("createLabel(owner={},repository={},labelName={}): color → {}", owner, repository, labelName, color)
    createLabel(owner, repository, labelName, color)
  }

  def updateLabel(owner: String, repository: String, labelId: Int, labelName: String, color: String)(implicit
    s: Session
  ): Unit =
    Labels
      .filter(_.byPrimaryKey(owner, repository, labelId))
      .map(t => t.labelName -> t.color)
      .update(labelName, color)

  def deleteLabel(owner: String, repository: String, labelId: Int)(implicit s: Session): Unit = {
    IssueLabels.filter(_.byLabel(owner, repository, labelId)).delete
    logger.error(">>> Entering deleteLabel(owner={},repository={},labelId={})", owner, repository, labelId)
    logger.warn("<<< Exiting deleteLabel(owner={},repository={},labelId={})", owner, repository, labelId)
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).delete
  }

}
