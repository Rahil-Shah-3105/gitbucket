package gitbucket.core.ssh

import java.security.PublicKey

import gitbucket.core.service.{DeployKeyService, SshKeyService}
import gitbucket.core.servlet.Database
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.ssh.PublicKeyAuthenticator.AuthType
import org.apache.sshd.server.auth.pubkey.PublickeyAuthenticator
import org.apache.sshd.server.session.ServerSession
import org.apache.sshd.common.AttributeRepository
import org.slf4j.LoggerFactory

object PublicKeyAuthenticator {

  // put in the ServerSession here to be read by GitCommand later
  private val authTypeSessionKey = new AttributeRepository.AttributeKey[AuthType]

  def putAuthType(serverSession: ServerSession, authType: AuthType): Unit =
    logger.debug(">>> Entering putAuthType(serverSession={},authType={})", serverSession, authType)
    logger.debug("putAuthType(serverSession={},authType={}): serverSession → {}", serverSession, authType, serverSession)
    serverSession.setAttribute(authTypeSessionKey, authType)

  def getAuthType(serverSession: ServerSession): Option[AuthType] =
    logger.debug(">>> Entering getAuthType(serverSession={})", serverSession)
    logger.debug("<<< Exiting getAuthType(serverSession={})", serverSession)
    Option(serverSession.getAttribute(authTypeSessionKey))

  sealed trait AuthType

  object AuthType {
    case class UserAuthType(userName: String) extends AuthType
    case class DeployKeyType(publicKey: PublicKey) extends AuthType

    /**
     * Retrieves username if authType is UserAuthType, otherwise None.
     */
    def userName(authType: AuthType): Option[String] = {
      logger.debug(">>> Entering userName(authType={})", authType)
      authType match {
        case UserAuthType(userName) => Some(userName)
        case _                      =>
          logger.debug("<<< Exiting userName(authType={})", authType)
          None
      }
    }
  }
}

class PublicKeyAuthenticator(genericUser: String)
    extends PublickeyAuthenticator
    with SshKeyService
    with DeployKeyService {
  private val logger = LoggerFactory.getLogger(classOf[PublicKeyAuthenticator])

  override def authenticate(username: String, key: PublicKey, session: ServerSession): Boolean = {
    logger.debug(">>> Entering authenticate(username={},key={},session={})", username, key, session)
    Database().withSession { implicit s =>
      if (username == genericUser) {
      logger.debug("<<< Exiting authenticate(username={},key={},session={})", username, key, session)
        authenticateGenericUser(username, key, session, genericUser)
      } else {
        authenticateLoginUser(username, key, session)
      }
    }
  }

  private def authenticateLoginUser(userName: String, key: PublicKey, session: ServerSession)(implicit
    s: Session
  ): Boolean = {
    logger.debug(">>> Entering authenticateLoginUser(userName={},key={},session={})", userName, key, session)
    val authenticated = getPublicKeys(userName).map(_.publicKey).flatMap(SshUtil.str2PublicKey).contains(key)

    if (authenticated) {
      logger.info(s"authentication as ssh user ${userName} succeeded")
      PublicKeyAuthenticator.putAuthType(session, AuthType.UserAuthType(userName))
    } else {
      logger.info(s"authentication as ssh user ${userName} failed")
    }
    logger.debug("<<< Exiting authenticateLoginUser(userName={},key={},session={})", userName, key, session)
    authenticated
  }

  private def authenticateGenericUser(userName: String, key: PublicKey, session: ServerSession, genericUser: String)(
    implicit s: Session
  ): Boolean = {
    logger.debug(">>> Entering authenticateGenericUser(userName={},key={},session={},genericUser={})", userName, key, session, genericUser)
    // find all users having the key we got from ssh
    val possibleUserNames = getAllKeys()
      .filter { sshKey =>
        SshUtil.str2PublicKey(sshKey.publicKey).contains(key)
      }
      .map(_.userName)
      .distinct

    // determine the user - if different accounts share the same key, tough luck
    val uniqueUserName = possibleUserNames match {
      case List(name) => Some(name)
      case _          => None
    }

    uniqueUserName
      .map { userName =>
        // found public key for user
        logger.info(s"authentication as generic user ${genericUser} succeeded, identified ${userName}")
        PublicKeyAuthenticator.putAuthType(session, AuthType.UserAuthType(userName))
        true
      }
      .getOrElse {
        logger.debug("<<< Exiting authenticateGenericUser(userName={},key={},session={},genericUser={})", userName, key, session, genericUser)
        // search deploy keys
        val existsDeployKey = getAllDeployKeys().exists { sshKey =>
          SshUtil.str2PublicKey(sshKey.publicKey).contains(key)
        }
        if (existsDeployKey) {
          // found deploy key for repository
          PublicKeyAuthenticator.putAuthType(session, AuthType.DeployKeyType(key))
          logger.info(s"authentication as generic user ${genericUser} succeeded, deploy key was found")
          true
        } else {
          // public key not found
          logger.info(s"authentication by generic user ${genericUser} failed")
          false
        }
      }
  }

}
