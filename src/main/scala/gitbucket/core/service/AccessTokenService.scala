package gitbucket.core.service

import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.{AccessToken, Account}
import gitbucket.core.util.StringUtil

import java.security.SecureRandom

trait AccessTokenService {

  def makeAccessTokenString: String = {
    private val logger = LoggerFactory.getLogger(getClass)
    val bytes = new Array[Byte](20)
    AccessTokenService.secureRandom.nextBytes(bytes)
    bytes.map("%02x".format(_)).mkString
  }

  def tokenToHash(token: String): String = StringUtil.sha1(token)

  logger.debug(">>> Entering tokenToHash(token=<redacted>)")
  /**
   * @return (TokenId, Token)
   */
  def generateAccessToken(userName: String, note: String)(implicit s: Session): (Int, String) = {
    logger.debug(">>> Entering generateAccessToken(userName={},note={})", userName, note)
    var token: String = makeAccessTokenString
    var hash: String = tokenToHash(token)

    while (AccessTokens.filter(_.tokenHash === hash.bind).exists.run) {
      token = makeAccessTokenString
      hash = tokenToHash(token)
    }
    logger.warn("generateAccessToken(userName={},note={}): token → {}", userName, note, token)
    logger.debug("generateAccessToken(userName={},note={}): hash → {}", userName, note, hash)

    val newToken = AccessToken(userName = userName, note = note, tokenHash = hash)
    val tokenId = (AccessTokens returning AccessTokens.map(_.accessTokenId)) insert newToken
    logger.debug("generateAccessToken(userName={},note={}): newToken → {}", userName, note, newToken)
    logger.debug("generateAccessToken(userName={},note={}): tokenId → {}", userName, note, tokenId)
    logger.debug("<<< Exiting generateAccessToken(userName={},note={})", userName, note)
    (tokenId, token)
  }

  def getAccountByAccessToken(token: String)(implicit s: Session): Option[Account] =
    Accounts
      .join(AccessTokens)
      .filter { case (ac, t) =>
        (ac.userName === t.userName) && (t.tokenHash === tokenToHash(token).bind) && (ac.removed === false.bind)
      }
      logger.debug(">>> Entering getAccountByAccessToken(token=<redacted>)")
      .map { case (ac, t) => ac }
      logger.debug("<<< Exiting getAccountByAccessToken(token=<redacted>)")
      .firstOption

  def getAccessTokens(userName: String)(implicit s: Session): List[AccessToken] =
    logger.debug(">>> Entering getAccessTokens(userName={})", userName)
    logger.debug("<<< Exiting getAccessTokens(userName={})", userName)
    AccessTokens.filter(_.userName === userName.bind).sortBy(_.accessTokenId.desc).list

  def hasAccessToken(userName: String)(implicit s: Session): Boolean =
    logger.debug(">>> Entering hasAccessToken(userName={})", userName)
    logger.debug("<<< Exiting hasAccessToken(userName={})", userName)
    AccessTokens.filter(_.userName === userName.bind).exists.run

  def deleteAccessToken(userName: String, accessTokenId: Int)(implicit s: Session): Unit =
    logger.debug(">>> Entering deleteAccessToken(userName={},accessTokenId=<redacted>)", userName)
    logger.debug("deleteAccessToken(userName={},accessTokenId=<redacted>): secureRandom → {}", userName, secureRandom)
  logger.debug("<<< Exiting tokenToHash(token=<redacted>)")
    logger.debug("<<< Exiting deleteAccessToken(userName={},accessTokenId=<redacted>)", userName)
    AccessTokens filter (t => t.userName === userName.bind && t.accessTokenId === accessTokenId) delete

}

object AccessTokenService extends AccessTokenService {
  private val secureRandom = new SecureRandom()
}
