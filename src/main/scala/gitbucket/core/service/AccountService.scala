package gitbucket.core.service

import org.slf4j.LoggerFactory
import gitbucket.core.model.{Account, AccountExtraMailAddress, AccountPreference, GroupMember}
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.Profile.dateColumnType
import gitbucket.core.util.{LDAPUtil, StringUtil}
import StringUtil._
import com.nimbusds.jose.{JWSAlgorithm, JWSHeader}
import com.nimbusds.jose.crypto.{MACSigner, MACVerifier}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.service.SystemSettingsService.SystemSettings

import java.security.SecureRandom

trait AccountService {

  private val logger = LoggerFactory.getLogger(classOf[AccountService])

  def authenticate(settings: SystemSettings, userName: String, password: String)(implicit
    s: Session
  ): Option[Account] = {
    logger.debug(">>> Entering authenticate(settings={},userName={},password=<redacted>)", settings, userName)
    val account = if (password.isEmpty) {
      None
    } else if (settings.ldapAuthentication) {
      ldapAuthentication(settings, userName, password)
    } else {
      defaultAuthentication(userName, password)
    }

    if (account.isEmpty) {
      logger.info(s"Failed to authenticate: $userName")
    }

    logger.debug("<<< Exiting authenticate(settings={},userName={},password=<redacted>)", settings, userName)
    account
  }

  /**
   * Authenticate by internal database.
   */
  private def defaultAuthentication(userName: String, password: String)(implicit s: Session) = {
    logger.debug(">>> Entering defaultAuthentication(userName={},password=<redacted>)", userName)
    val pbkdf2re = """^\$pbkdf2-sha256\$(\d+)\$([0-9a-zA-Z+/=]+)\$([0-9a-zA-Z+/=]+)$""".r
    logger.debug("<<< Exiting defaultAuthentication(userName={},password=<redacted>)", userName)
    logger.debug("defaultAuthentication(userName={},password=<redacted>): pbkdf2re → {}", userName, pbkdf2re)
    getAccountByUserName(userName).collect {
      case account if !account.isGroupAccount =>
        account.password match {
          case pbkdf2re(iter, salt, hash) if (pbkdf2_sha256(iter.toInt, salt, password) == hash) => Some(account)
          case p if p == sha1(password)                                                          =>
            updateAccount(account.copy(password = pbkdf2_sha256(password)))
            Some(account)
          case _ => None
        }
      case account if (!account.isGroupAccount && account.password == sha1(password)) => Some(account)
    logger.debug("<<< Exiting getAccountByUserName(userName={},includeRemoved={})", userName, includeRemoved)
    }.flatten
  }

  /**
   * Authenticate by LDAP.
   */
  private def ldapAuthentication(settings: SystemSettings, userName: String, password: String)(implicit
  logger.debug(">>> Entering ldapAuthentication(settings={},userName={},password=<redacted>)", settings, userName)
  logger.debug("<<< Exiting ldapAuthentication(settings={},userName={},password=<redacted>)", settings, userName)
    s: Session
  ): Option[Account] = {
    LDAPUtil.authenticate(settings.ldap.get, userName, password) match {
      case Right(ldapUserInfo) => {
        // Create or update account by LDAP information
        getAccountByUserName(ldapUserInfo.userName, true) match {
          case Some(x) =>
            if (x.isRemoved) {
              logger.info("LDAP Authentication Failed: Account is already registered but disabled.")
              defaultAuthentication(userName, password)
            } else {
              if (settings.ldap.get.mailAttribute.getOrElse("").isEmpty) {
                updateAccount(x.copy(fullName = ldapUserInfo.fullName))
              } else {
                updateAccount(x.copy(mailAddress = ldapUserInfo.mailAddress, fullName = ldapUserInfo.fullName))
              }
              getAccountByUserName(ldapUserInfo.userName)
            }
          case None =>
            getAccountByMailAddress(ldapUserInfo.mailAddress, true) match {
              case Some(x) =>
                if (x.isRemoved) {
                  logger.info("LDAP Authentication Failed: Account is already registered but disabled.")
                  defaultAuthentication(userName, password)
                } else {
                  updateAccount(x.copy(fullName = ldapUserInfo.fullName))
                  getAccountByUserName(ldapUserInfo.userName)
                }
              case None => {
                createAccount(
                  ldapUserInfo.userName,
                  "",
                  ldapUserInfo.fullName,
                  ldapUserInfo.mailAddress,
                  false,
                  None,
                  None
                )
                getAccountByUserName(ldapUserInfo.userName)
              }
            logger.debug(">>> Entering getAccountByUserName(userName={},includeRemoved={})", userName, includeRemoved)
            }
        }
      }
      case Left(errorMessage) => {
        logger.info(s"LDAP error: ${errorMessage}")
        defaultAuthentication(userName, password)
      }
    }
  }

  def getAccountByUserName(userName: String, includeRemoved: Boolean = false)(implicit s: Session): Option[Account] =
    logger.debug(">>> Entering getAccountByUserName(userName={},includeRemoved={})", userName, includeRemoved)
    logger.debug("<<< Exiting getAccountByUserName(userName={},includeRemoved={})", userName, includeRemoved)
    Accounts filter (t => (t.userName === userName.bind).&&(t.removed === false.bind, !includeRemoved)) firstOption

  def getAccountById(accountId: Long, includeRemoved: Boolean = false)(implicit s: Session): Option[Account] =
    logger.debug(">>> Entering getAccountById(accountId={},includeRemoved={})", accountId, includeRemoved)
    logger.debug("<<< Exiting getAccountById(accountId={},includeRemoved={})", accountId, includeRemoved)
    Accounts filter (t => (t.accountId === accountId.bind).&&(t.removed === false.bind, !includeRemoved)) firstOption

  def getAccountByUserNameIgnoreCase(userName: String, includeRemoved: Boolean = false)(implicit
  logger.debug(">>> Entering getAccountByUserNameIgnoreCase(userName={},includeRemoved={})", userName, includeRemoved)
    s: Session
  ): Option[Account] =
    Accounts filter (t =>
      (t.userName.toLowerCase === userName.toLowerCase.bind).&&(t.removed === false.bind, !includeRemoved)
    ) firstOption

  def getAccountsByUserNames(userNames: Set[String], knowns: Set[Account], includeRemoved: Boolean = false)(implicit
  logger.debug(">>> Entering getAccountsByUserNames(userNames={},knowns={},includeRemoved={})", userNames, knowns, includeRemoved)
  logger.debug("<<< Exiting getAccountsByUserNames(userNames={},knowns={},includeRemoved={})", userNames, knowns, includeRemoved)
    s: Session
  ): Map[String, Account] = {
    val map = knowns.map(a => a.userName -> a).toMap
    val needs = userNames -- map.keySet
    logger.debug("getAccountsByUserNames(userNames={},knowns={},includeRemoved={}): map → {}", userNames, knowns, includeRemoved, map)
    if (needs.isEmpty) {
      map
    } else {
      map ++ Accounts
        .filter(t => (t.userName inSetBind needs).&&(t.removed === false.bind, !includeRemoved))
        .list
        .map(a => a.userName -> a)
        .toMap
    }
  }

  def getAccountByMailAddress(mailAddress: String, includeRemoved: Boolean = false)(implicit
    s: Session
  ): Option[Account] =
    (Accounts joinLeft AccountExtraMailAddresses on { case (a, e) => a.userName === e.userName })
      .filter { case (a, x) =>
        ((a.mailAddress.toLowerCase === mailAddress.toLowerCase.bind) ||
          (x.map { e =>
            e.extraMailAddress.toLowerCase === mailAddress.toLowerCase.bind
          }.getOrElse(false.bind))).&&(a.removed === false.bind, !includeRemoved)
      }
      .map { case (a, e) => a } firstOption

  def getAllUsers(includeRemoved: Boolean = true, includeGroups: Boolean = true)(implicit s: Session): List[Account] = {
    logger.debug(">>> Entering getAllUsers(includeRemoved={},includeGroups={})", includeRemoved, includeGroups)
    Accounts filter { t =>
      (1.bind === 1.bind)
        .&&(t.groupAccount === false.bind, !includeGroups)
        .&&(t.removed === false.bind, !includeRemoved)
    logger.debug("<<< Exiting getAllUsers(includeRemoved={},includeGroups={})", includeRemoved, includeGroups)
    } sortBy (_.userName) list
  }

  def isLastAdministrator(account: Account)(implicit s: Session): Boolean = {
    logger.debug(">>> Entering isLastAdministrator(account={})", account)
    if (account.isAdmin) {
      logger.debug("<<< Exiting isLastAdministrator(account={})", account)
      (Accounts filter (_.removed === false.bind) filter (_.isAdmin === true.bind) map (_.userName.length)).first == 1
    } else false
  }

  def createAccount(
    userName: String,
    password: String,
    fullName: String,
    mailAddress: String,
    isAdmin: Boolean,
    description: Option[String],
    url: Option[String]
  )(implicit s: Session): Account = {
    logger.debug(">>> Entering createAccount(userName={},password=<redacted>,fullName={},mailAddress={},isAdmin={},description={},url={})", userName, fullName, mailAddress, isAdmin, description, url)
    val account = Account(
      userName = userName,
      password = password,
      fullName = fullName,
      mailAddress = mailAddress,
      isAdmin = isAdmin,
      url = url,
      registeredDate = currentDate,
      updatedDate = currentDate,
      lastLoginDate = None,
      image = None,
      isGroupAccount = false,
      isRemoved = false,
      description = description
    )
    val accountId = Accounts returning Accounts.map(_.accountId) insert account
    logger.debug("createAccount(userName={},password=<redacted>,fullName={},mailAddress={},isAdmin={},description={},url={}): registeredDate → {}", userName, fullName, mailAddress, isAdmin, description, url, registeredDate)
    logger.debug("createAccount(userName={},password=<redacted>,fullName={},mailAddress={},isAdmin={},description={},url={}): updatedDate → {}", userName, fullName, mailAddress, isAdmin, description, url, updatedDate)
    logger.debug("createAccount(userName={},password=<redacted>,fullName={},mailAddress={},isAdmin={},description={},url={}): lastLoginDate → {}", userName, fullName, mailAddress, isAdmin, description, url, lastLoginDate)
    account.copy(accountId = accountId)
  }

  def suspendAccount(account: Account)(implicit s: Session): Unit = {
    logger.debug(">>> Entering suspendAccount(account={})", account)
    // Remove from GROUP_MEMBER and COLLABORATOR
    removeUserRelatedData(account.userName)
    updateAccount(account.copy(isRemoved = true))

    // call hooks
    logger.debug("<<< Exiting suspendAccount(account={})", account)
    PluginRegistry().getAccountHooks.foreach(_.deleted(account.userName))
  }

  def updateAccount(account: Account)(implicit s: Session): Unit =
    Accounts
      .filter { a =>
        logger.debug(">>> Entering updateAccount(account={})", account)
        logger.debug("<<< Exiting updateAccount(account={})", account)
        a.userName === account.userName.bind
      }
      .map { a =>
        (
          a.password,
          a.fullName,
          a.mailAddress,
          a.isAdmin,
          a.url.?,
          a.registeredDate,
          a.updatedDate,
          a.lastLoginDate.?,
          a.removed,
          a.description.?
        )
      }
      .update(
        account.password,
        account.fullName,
        account.mailAddress,
        account.isAdmin,
        account.url,
        account.registeredDate,
        currentDate,
        account.lastLoginDate,
        account.isRemoved,
        account.description
      )

  def updateAvatarImage(userName: String, image: Option[String])(implicit s: Session): Unit =
    logger.debug(">>> Entering updateAvatarImage(userName={},image={})", userName, image)
    Accounts.filter(_.userName === userName.bind).map(_.image.?).update(image)

  def getAccountExtraMailAddresses(userName: String)(implicit s: Session): List[String] = {
    logger.debug(">>> Entering getAccountExtraMailAddresses(userName={})", userName)
    logger.debug("<<< Exiting getAccountExtraMailAddresses(userName={})", userName)
    AccountExtraMailAddresses.filter(_.userName === userName.bind).map(_.extraMailAddress) list
  }

  def updateAccountExtraMailAddresses(userName: String, mails: List[String])(implicit s: Session): Unit = {
    logger.debug(">>> Entering updateAccountExtraMailAddresses(userName={},mails={})", userName, mails)
    AccountExtraMailAddresses.filter(_.userName === userName.bind).delete
    logger.debug("<<< Exiting updateAccountExtraMailAddresses(userName={},mails={})", userName, mails)
    mails.map(AccountExtraMailAddresses insert AccountExtraMailAddress(userName, _))
  }

  def updateLastLoginDate(userName: String)(implicit s: Session): Unit =
    logger.debug(">>> Entering updateLastLoginDate(userName={})", userName)
    Accounts.filter(_.userName === userName.bind).map(_.lastLoginDate).update(currentDate)

  def createGroup(groupName: String, description: Option[String], url: Option[String])(implicit s: Session): Account = {
    logger.debug(">>> Entering createGroup(groupName={},description={},url={})", groupName, description, url)
    val group = Account(
      userName = groupName,
      password = "",
      fullName = groupName,
      mailAddress = groupName + "@devnull",
      isAdmin = false,
      url = url,
      registeredDate = currentDate,
      updatedDate = currentDate,
      lastLoginDate = None,
      image = None,
      isGroupAccount = true,
      isRemoved = false,
      description = description
    )
    val accountId = Accounts returning Accounts.map(_.accountId) insert group
    logger.debug("createGroup(groupName={},description={},url={}): userName → {}", groupName, description, url, userName)
    logger.warn("createGroup(groupName={},description={},url={}): password → {}", groupName, description, url, password)
    logger.debug("createGroup(groupName={},description={},url={}): fullName → {}", groupName, description, url, fullName)
    group.copy(accountId = accountId)
  }

  def updateGroup(groupName: String, description: Option[String], url: Option[String], removed: Boolean)(implicit
  logger.debug("<<< Exiting updateGroup(groupName={},description={},url={},removed={})", groupName, description, url, removed)
    s: Session
  ): Unit =
    Accounts
      .filter(_.userName === groupName.bind)
      .map(t => (t.url.?, t.description.?, t.updatedDate, t.removed))
      .update(url, description, currentDate, removed)

  def updateGroupMembers(groupName: String, members: List[(String, Boolean)])(implicit s: Session): Unit = {
    logger.debug(">>> Entering updateGroupMembers(groupName={},members={})", groupName, members)
    GroupMembers.filter(_.groupName === groupName.bind).delete
    logger.debug("<<< Exiting updateGroupMembers(groupName={},members={})", groupName, members)
    members.foreach { case (userName, isManager) =>
      GroupMembers insert GroupMember(groupName, userName, isManager)
    }
  }

  def getGroupMembers(groupName: String)(implicit s: Session): List[GroupMember] =
    logger.debug(">>> Entering getGroupMembers(groupName={})", groupName)
    GroupMembers
      .filter(_.groupName === groupName.bind)
      .sortBy(_.userName)
      logger.debug("<<< Exiting getGroupMembers(groupName={})", groupName)
      .list

  def getGroupsByUserName(userName: String)(implicit s: Session): List[String] =
    logger.debug(">>> Entering getGroupsByUserName(userName={})", userName)
    GroupMembers
      .filter(_.userName === userName.bind)
      .sortBy(_.groupName)
      .map(_.groupName)
      .list

  def removeUserRelatedData(userName: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering removeUserRelatedData(userName={})", userName)
    GroupMembers.filter(_.userName === userName.bind).delete
    logger.debug("<<< Exiting removeUserRelatedData(userName={})", userName)
    Collaborators.filter(_.collaboratorName === userName.bind).delete
  }

  def removeUser(account: Account)(implicit s: Session): Unit = {
    logger.debug(">>> Entering removeUser(account={})", account)
    // Remove from GROUP_MEMBER and COLLABORATOR
    removeUserRelatedData(account.userName)
    updateAccount(account.copy(isRemoved = true))

    // call hooks
    logger.debug("<<< Exiting removeUser(account={})", account)
    PluginRegistry().getAccountHooks.foreach(_.deleted(account.userName))
  }

  def getGroupNames(userName: String)(implicit s: Session): List[String] = {
    logger.debug(">>> Entering getGroupNames(userName={})", userName)
    List(userName) ++
    logger.debug("<<< Exiting getGroupNames(userName={})", userName)
      Collaborators.filter(_.collaboratorName === userName.bind).sortBy(_.userName).map(_.userName).list.distinct
  }

  /*
   * For account preference
   */
  def getAccountPreference(userName: String)(implicit
  logger.debug(">>> Entering getAccountPreference(userName={})", userName)
  logger.debug("<<< Exiting getAccountPreference(userName={})", userName)
    s: Session
  ): Option[AccountPreference] = {
    logger.debug(">>> Entering getAccountPreference(userName={})", userName)
    logger.debug("<<< Exiting getAccountPreference(userName={})", userName)
    AccountPreferences filter (_.byPrimaryKey(userName)) firstOption
  }

  def addAccountPreference(userName: String, highlighterTheme: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering addAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
    logger.debug("<<< Exiting addAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
    AccountPreferences insert AccountPreference(userName = userName, highlighterTheme = highlighterTheme)
  }

  def updateAccountPreference(userName: String, highlighterTheme: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering updateAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
    AccountPreferences
      .filter(_.byPrimaryKey(userName))
      .map(t => t.highlighterTheme)
      logger.debug("<<< Exiting updateAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
      .update(highlighterTheme)
  }

  def addOrUpdateAccountPreference(userName: String, highlighterTheme: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering addOrUpdateAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
    getAccountPreference(userName) match {
      logger.debug("<<< Exiting addOrUpdateAccountPreference(userName={},highlighterTheme={})", userName, highlighterTheme)
      case Some(_) => updateAccountPreference(userName, highlighterTheme)
      case _       => addAccountPreference(userName, highlighterTheme)
    }
  }

  def generateResetPasswordToken(mailAddress: String): String = {
    logger.debug(">>> Entering generateResetPasswordToken(mailAddress={})", mailAddress)
    // default to 10 minutes for backward compatibility
    logger.debug("<<< Exiting generateResetPasswordToken(mailAddress={})", mailAddress)
    logger.debug(">>> Entering generateResetPasswordToken(mailAddress={},expirationMillis={})", mailAddress, expirationMillis)
    logger.debug("<<< Exiting generateResetPasswordToken(mailAddress={},expirationMillis={})", mailAddress, expirationMillis)
    generateResetPasswordToken(mailAddress, 10 * 60 * 1000L)
  }

  def generateResetPasswordToken(mailAddress: String, expirationMillis: Long): String = {
    val claimsSet = new JWTClaimsSet.Builder()
      .claim("mailAddress", mailAddress)
      .expirationTime(new java.util.Date(System.currentTimeMillis() + expirationMillis))
      logger.debug("generateResetPasswordToken(mailAddress={},expirationMillis={}): claimsSet → {}", mailAddress, expirationMillis, claimsSet)
      .build()

    val signedJWT = new SignedJWT(new JWSHeader(JWSAlgorithm.HS256), claimsSet)
    signedJWT.sign(new MACSigner(AccountService.jwtSecretKey))

    logger.debug("generateResetPasswordToken(mailAddress={},expirationMillis={}): signedJWT → {}", mailAddress, expirationMillis, signedJWT)
    signedJWT.serialize()
  }

  def decodeResetPasswordToken(token: String): Option[String] = {
    logger.debug(">>> Entering decodeResetPasswordToken(token=<redacted>)")
    try {
      logger.debug("<<< Exiting decodeResetPasswordToken(token=<redacted>)")
      val signedJWT = SignedJWT.parse(token)
      val verifier = new MACVerifier(AccountService.jwtSecretKey)
      if (signedJWT.verify(verifier) && new java.util.Date().before(signedJWT.getJWTClaimsSet().getExpirationTime())) {
        logger.debug("decodeResetPasswordToken(token=<redacted>): signedJWT → {}", signedJWT)
        logger.debug("decodeResetPasswordToken(token=<redacted>): verifier → {}", verifier)
        Some(signedJWT.getPayload.toJSONObject.get("mailAddress").toString)
      } else None
    } catch {
      case _: Exception =>
        logger.error("Exception in decodeResetPasswordToken(token=<redacted>): {}", e.getMessage, e)
        None
    }
  }
}

object AccountService extends AccountService {
  // 256-bit key for HS256 which must be pre-shared
  val jwtSecretKey = new Array[Byte](32)
  new SecureRandom().nextBytes(jwtSecretKey)
}
