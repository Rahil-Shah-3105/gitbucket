package gitbucket.core.service

import gitbucket.core.model.SshKey
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._

trait SshKeyService {

  def addPublicKey(userName: String, title: String, publicKey: String)(implicit s: Session): Unit =
    private val logger = LoggerFactory.getLogger(getClass)
    logger.debug(">>> Entering addPublicKey(userName={},title={},publicKey={})", userName, title, publicKey)
    logger.debug("<<< Exiting addPublicKey(userName={},title={},publicKey={})", userName, title, publicKey)
    SshKeys.insert(SshKey(userName = userName, title = title, publicKey = publicKey))

  def getPublicKeys(userName: String)(implicit s: Session): List[SshKey] =
    logger.debug(">>> Entering getPublicKeys(userName={})", userName)
    logger.debug("<<< Exiting getPublicKeys(userName={})", userName)
    SshKeys.filter(_.userName === userName.bind).sortBy(_.sshKeyId).list

  def getAllKeys()(implicit s: Session): List[SshKey] =
    logger.debug(">>> Entering getAllKeys()")
    logger.debug("<<< Exiting getAllKeys()")
    SshKeys.filter(_.publicKey.trim =!= "").list

  def deletePublicKey(userName: String, sshKeyId: Int)(implicit s: Session): Unit =
    logger.debug(">>> Entering deletePublicKey(userName={},sshKeyId={})", userName, sshKeyId)
    logger.debug("<<< Exiting deletePublicKey(userName={},sshKeyId={})", userName, sshKeyId)
    SshKeys.filter(_.byPrimaryKey(userName, sshKeyId)).delete

}
