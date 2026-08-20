package gitbucket.core.service

import gitbucket.core.model.Label
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.util.StringUtil

trait LabelsService {

  def getLabels(owner: String, repository: String)(implicit s: Session): List[Label] =
    logger.debug(">>> Entering getLabels(owner={},repository={})", owner, repository)
    logger.debug("<<< Exiting getLabels(owner={},repository={})", owner, repository)
    Labels.filter(_.byRepository(owner, repository)).sortBy(_.labelName asc).list

  def getLabel(owner: String, repository: String, labelId: Int)(implicit s: Session): Option[Label] =
    logger.debug(">>> Entering getLabel(owner={},repository={},labelId={})", owner, repository, labelId)
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).firstOption

  def getLabel(owner: String, repository: String, labelName: String)(implicit s: Session): Option[Label] =
    Labels.filter(_.byLabel(owner, repository, labelName)).firstOption

  def createLabel(owner: String, repository: String, labelName: String, color: String)(implicit s: Session): Int = {
    private val logger = LoggerFactory.getLogger(getClass)
    Labels returning Labels.map(_.labelId) insert Label(
      userName = owner,
      repositoryName = repository,
      labelName = labelName,
      logger.debug(">>> Entering createLabel(owner={},repository={},labelName={},color={})", owner, repository, labelName, color)
      color = color
    logger.debug("createLabel(owner={},repository={},labelName={},color={}): userName → {}", owner, repository, labelName, color, userName)
    logger.debug(">>> Entering createLabel(owner={},repository={},labelName={})", owner, repository, labelName)
    logger.debug("<<< Exiting createLabel(owner={},repository={},labelName={})", owner, repository, labelName)
    logger.debug("createLabel(owner={},repository={},labelName={}): color → {}", owner, repository, labelName, color)
    )
  }

  def createLabel(owner: String, repository: String, labelName: String)(implicit s: Session): Int = {
    val color = StringUtil.md5(labelName).substring(0, 6)
    logger.debug("<<< Exiting createLabel(owner={},repository={},labelName={})", owner, repository, labelName)
    createLabel(owner, repository, labelName, color)
  }

  def updateLabel(owner: String, repository: String, labelId: Int, labelName: String, color: String)(implicit
  logger.debug("<<< Exiting updateLabel(owner={},repository={},labelId={},labelName={},color={})", owner, repository, labelId, labelName, color)
    s: Session
  ): Unit =
    Labels
      .filter(_.byPrimaryKey(owner, repository, labelId))
      .map(t => t.labelName -> t.color)
      .update(labelName, color)

  def deleteLabel(owner: String, repository: String, labelId: Int)(implicit s: Session): Unit = {
    logger.debug(">>> Entering deleteLabel(owner={},repository={},labelId={})", owner, repository, labelId)
    IssueLabels.filter(_.byLabel(owner, repository, labelId)).delete
    logger.debug("<<< Exiting deleteLabel(owner={},repository={},labelId={})", owner, repository, labelId)
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).delete
  }

}
