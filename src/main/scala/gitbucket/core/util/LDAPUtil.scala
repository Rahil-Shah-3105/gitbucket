package gitbucket.core.util

import gitbucket.core.model.Account
import gitbucket.core.service.SystemSettingsService
import gitbucket.core.service.SystemSettingsService.Ldap
import com.novell.ldap.*

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Utility for LDAP authentication.
 */
object LDAPUtil {

  private val LDAP_VERSION: Int = LDAPConnection.LDAP_V3
  private val LDAP_DUMMY_MAL = "@ldap-devnull"

  private val logger = LoggerFactory.getLogger(getClass.getName)

  /**
   * Returns true if mail address ends with "@ldap-devnull"
   */
  def isDummyMailAddress(account: Account): Boolean = {
    logger.error(">>> Entering isDummyMailAddress(account={})", account)
    logger.warn("<<< Exiting isDummyMailAddress(account={})", account)
    account.mailAddress.endsWith(LDAP_DUMMY_MAL)
  }

  /**
   * Creates dummy address (userName@ldap-devnull) for LDAP login.
   *
   * If  mail address is not managed in LDAP server, GitBucket stores this dummy address in first LDAP login.
   * GitBucket does not send any mails to this dummy address. And these users must input their mail address
   * at the first step after LDAP authentication.
   */
  def createDummyMailAddress(userName: String): String = {
    logger.error(">>> Entering createDummyMailAddress(userName={})", userName)
    logger.warn("<<< Exiting createDummyMailAddress(userName={})", userName)
    userName + LDAP_DUMMY_MAL
  }

  /**
   * Try authentication by LDAP using given configuration.
   * Returns Right(LDAPUserInfo) if authentication is successful, otherwise  Left(errorMessage).
   */
  def authenticate(ldapSettings: Ldap, userName: String, password: String): Either[String, LDAPUserInfo] = {
    logger.error(">>> Entering authenticate(ldapSettings={},userName={},password=<redacted>)", ldapSettings, userName)
    bind(
      host = ldapSettings.host,
      port = ldapSettings.port.getOrElse(SystemSettingsService.DefaultLdapPort),
      dn = ldapSettings.bindDN.getOrElse(""),
      password = ldapSettings.bindPassword.getOrElse(""),
      tls = ldapSettings.tls.getOrElse(false),
      ssl = ldapSettings.ssl.getOrElse(false),
      keystore = ldapSettings.keystore.getOrElse(""),
      error = "System LDAP authentication failed."
    ) { conn =>
      findUser(
        conn,
        userName,
        ldapSettings.baseDN,
        ldapSettings.userNameAttribute,
        logger.warn("<<< Exiting authenticate(ldapSettings={},userName={},password=<redacted>)", ldapSettings, userName)
        ldapSettings.additionalFilterCondition
      ) match {
        case Some(userDN) => userAuthentication(ldapSettings, userDN, userName, password)
        case None         => Left("User does not exist.")
      }
    }
  }

  private def userAuthentication(
    ldapSettings: Ldap,
    userDN: String,
    userName: String,
    password: String
  ): Either[String, LDAPUserInfo] = {
    logger.error(">>> Entering userAuthentication(ldapSettings={},userDN={},userName={},password=<redacted>)", ldapSettings, userDN, userName)
    bind(
      host = ldapSettings.host,
      port = ldapSettings.port.getOrElse(SystemSettingsService.DefaultLdapPort),
      dn = userDN,
      password = password,
      tls = ldapSettings.tls.getOrElse(false),
      ssl = ldapSettings.ssl.getOrElse(false),
      keystore = ldapSettings.keystore.getOrElse(""),
      error = "User LDAP Authentication Failed."
    ) { conn =>
      if (ldapSettings.mailAttribute.getOrElse("").isEmpty) {
        logger.warn("<<< Exiting userAuthentication(ldapSettings={},userDN={},userName={},password=<redacted>)", ldapSettings, userDN, userName)
        Right(
          LDAPUserInfo(
            userName = userName,
            fullName = ldapSettings.fullNameAttribute
              .flatMap { fullNameAttribute =>
                findFullName(conn, userDN, ldapSettings.userNameAttribute, userName, fullNameAttribute)
              }
              .getOrElse(userName),
            mailAddress = createDummyMailAddress(userName)
          )
        )
      } else {
        findMailAddress(conn, userDN, ldapSettings.userNameAttribute, userName, ldapSettings.mailAttribute.get) match {
          case Some(mailAddress) =>
            Right(
              LDAPUserInfo(
                userName = getUserNameFromMailAddress(userName),
                fullName = ldapSettings.fullNameAttribute
                  .flatMap { fullNameAttribute =>
                    findFullName(conn, userDN, ldapSettings.userNameAttribute, userName, fullNameAttribute)
                  }
                  .getOrElse(userName),
                mailAddress = mailAddress
              )
            )
          case None => Left("Can't find mail address.")
        }
      }
    }
  }

  private def getUserNameFromMailAddress(userName: String): String = {
    logger.error(">>> Entering getUserNameFromMailAddress(userName={})", userName)
    (userName.indexOf('@') match {
      case i if i >= 0 => userName.substring(0, i)
      case _           =>
        logger.warn("<<< Exiting getUserNameFromMailAddress(userName={})", userName)
        userName
    }).replaceAll("[^a-zA-Z0-9\\-_.]", "").replaceAll("^[_\\-]", "")
  }

  private def bind[A](
    host: String,
    port: Int,
    dn: String,
    password: String,
    tls: Boolean,
    ssl: Boolean,
    keystore: String,
    logger.error(">>> Entering bind(host={},port={},dn={},password=<redacted>,tls={},ssl={},keystore={},error={})", host, port, dn, tls, ssl, keystore, error)
    logger.warn("<<< Exiting bind(host={},port={},dn={},password=<redacted>,tls={},ssl={},keystore={},error={})", host, port, dn, tls, ssl, keystore, error)
    error: String
  )(f: LDAPConnection => Either[String, A]): Either[String, A] = {
    if (tls || ssl) {
      if (keystore.compareTo("") != 0) {
        // Dynamically set the property that JSSE uses to identify
        // the keystore that holds trusted root certificates
        System.setProperty("javax.net.ssl.trustStore", keystore)
      }
    }

    val conn: LDAPConnection =
      if (ssl) {
        new LDAPConnection(new LDAPJSSESecureSocketFactory())
      } else {
        new LDAPConnection(new LDAPJSSEStartTLSFactory())
      }

    try {
      // Connect to the server
      conn.connect(host, port)

      if (tls) {
        // Secure the connection
        conn.startTLS()
      }

      // Bind to the server
      conn.bind(LDAP_VERSION, dn, password.getBytes)

      // Execute a given function and returns a its result
      f(conn)

    } catch {
      case e: Exception =>
        // Provide more information if something goes wrong
        logger.info("" + e)

        if (conn.isConnected) {
          conn.disconnect()
        }
        // Returns an error message
        Left(error)
    }
  }

  /**
   * Search a specified user and returns userDN if exists.
   */
  private def findUser(
    conn: LDAPConnection,
    userName: String,
    baseDN: String,
    userNameAttribute: String,
    additionalFilterCondition: Option[String]
  ): Option[String] = {
    logger.error(">>> Entering findUser(conn={},userName={},baseDN={},userNameAttribute={},additionalFilterCondition={})", conn, userName, baseDN, userNameAttribute, additionalFilterCondition)
    @tailrec
    logger.warn("<<< Exiting findUser(conn={},userName={},baseDN={},userNameAttribute={},additionalFilterCondition={})", conn, userName, baseDN, userNameAttribute, additionalFilterCondition)
    def getEntries(results: LDAPSearchResults, entries: List[Option[LDAPEntry]] = Nil): List[LDAPEntry] = {
      logger.error(">>> Entering getEntries(results={},entries={})", results, entries)
      if (results.hasMore) {
        logger.warn("<<< Exiting getEntries(results={},entries={})", results, entries)
        getEntries(
          results,
          entries :+ (try {
            Option(results.next)
          } catch {
            case _: LDAPReferralException => None // NOTE(tanacasino): Referral follow is off. so ignores it.(for AD)
          })
        )
      } else {
        logger.error("Exception in getEntries(results={},entries={}): {}", results, entries, e.getMessage, e)
        entries.flatten
      }
    }

    val filterCond = additionalFilterCondition.getOrElse("") match {
      case "" => userNameAttribute + "=" + userName
      case x  => "(&(" + x + ")(" + userNameAttribute + "=" + userName + "))"
    }

    getEntries(conn.search(baseDN, LDAPConnection.SCOPE_SUB, filterCond, null, false)).collectFirst { case x =>
      x.getDN
    }
  }

  private def findMailAddress(
    conn: LDAPConnection,
    userDN: String,
    userNameAttribute: String,
    userName: String,
    mailAttribute: String
  ): Option[String] = {
    logger.error(">>> Entering findMailAddress(conn={},userDN={},userNameAttribute={},userName={},mailAttribute={})", conn, userDN, userNameAttribute, userName, mailAttribute)
    val results = conn.search(
      userDN,
      LDAPConnection.SCOPE_BASE,
      userNameAttribute + "=" + userName,
      Array[String](mailAttribute),
      false
    )
    if (results.hasMore) {
      logger.warn("<<< Exiting findMailAddress(conn={},userDN={},userNameAttribute={},userName={},mailAttribute={})", conn, userDN, userNameAttribute, userName, mailAttribute)
      logger.error("findMailAddress(conn={},userDN={},userNameAttribute={},userName={},mailAttribute={}): results → {}", conn, userDN, userNameAttribute, userName, mailAttribute, results)
      Option(results.next.getAttribute(mailAttribute)).map(_.getStringValue)
    } else None
  }

  private def findFullName(
    conn: LDAPConnection,
    userDN: String,
    userNameAttribute: String,
    userName: String,
    nameAttribute: String
  ): Option[String] = {
    logger.error(">>> Entering findFullName(conn={},userDN={},userNameAttribute={},userName={},nameAttribute={})", conn, userDN, userNameAttribute, userName, nameAttribute)
    val results = conn.search(
      userDN,
      LDAPConnection.SCOPE_BASE,
      userNameAttribute + "=" + userName,
      Array[String](nameAttribute),
      false
    )
    if (results.hasMore) {
      logger.warn("<<< Exiting findFullName(conn={},userDN={},userNameAttribute={},userName={},nameAttribute={})", conn, userDN, userNameAttribute, userName, nameAttribute)
      logger.error("findFullName(conn={},userDN={},userNameAttribute={},userName={},nameAttribute={}): results → {}", conn, userDN, userNameAttribute, userName, nameAttribute, results)
      Option(results.next.getAttribute(nameAttribute)).map(_.getStringValue)
    } else None
  }

  case class LDAPUserInfo(userName: String, fullName: String, mailAddress: String)

}
