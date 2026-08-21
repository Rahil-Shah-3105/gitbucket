package gitbucket.core.service

import gitbucket.core.model.DeployKey
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._

trait DeployKeyService {

  def addDeployKey(userName: String, repositoryName: String, title: String, publicKey: String, allowWrite: Boolean)(
  private val logger = LoggerFactory.getLogger(getClass)
  logger.debug("<<< Exiting addDeployKey(userName={},repositoryName={},title={},publicKey={},allowWrite={})", userName, repositoryName, title, publicKey, allowWrite)
    implicit s: Session
  ): Unit =
    DeployKeys.insert(
      DeployKey(
        userName = userName,
        repositoryName = repositoryName,
        title = title,
        publicKey = publicKey,
        allowWrite = allowWrite
      )
    )

  def getDeployKeys(userName: String, repositoryName: String)(implicit s: Session): List[DeployKey] =
    logger.debug(">>> Entering getDeployKeys(userName={},repositoryName={})", userName, repositoryName)
    DeployKeys
      .filter(x => (x.userName === userName.bind) && (x.repositoryName === repositoryName.bind))
      .sortBy(_.deployKeyId)
      logger.debug("<<< Exiting getDeployKeys(userName={},repositoryName={})", userName, repositoryName)
      .list

  def getAllDeployKeys()(implicit s: Session): List[DeployKey] =
    logger.debug(">>> Entering getAllDeployKeys()")
    logger.debug("<<< Exiting getAllDeployKeys()")
    DeployKeys.filter(_.publicKey.trim =!= "").list

  def deleteDeployKey(userName: String, repositoryName: String, deployKeyId: Int)(implicit s: Session): Unit =
    logger.debug(">>> Entering deleteDeployKey(userName={},repositoryName={},deployKeyId={})", userName, repositoryName, deployKeyId)
    logger.debug("<<< Exiting deleteDeployKey(userName={},repositoryName={},deployKeyId={})", userName, repositoryName, deployKeyId)
    DeployKeys.filter(_.byPrimaryKey(userName, repositoryName, deployKeyId)).delete

}
