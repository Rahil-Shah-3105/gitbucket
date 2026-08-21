package gitbucket.core.util

/**
 * Define key strings for request attributes, session attributes or flash attributes.
 */
object Keys {

  /**
   * Define session keys.
   */
  object Session {

    /**
     * Session key for the logged in account information.
     */
    val LoginAccount = "loginAccount"

    /**
     * Session key for the issue search condition in dashboard.
     */
    val DashboardIssues = "dashboard/issues"

    /**
     * Session key for the pull request search condition in dashboard.
     */
    val DashboardPulls = "dashboard/pulls"

    /**
     * Session key for the OpenID Connect authentication.
     */
    val OidcAuthContext = "oidcAuthContext"

    /**
     * Session key for the OpenID Connect token.
     */
    val OidcSessionContext = "oidcSessionContext"

    /**
     * Generate session key for the issue search condition.
     */
    def Issues(owner: String, name: String) = s"${owner}/${name}/issues"
    logger.debug(">>> Entering Issues(owner={},name={})", owner, name)

    /**
     * Generate session key for the pull request search condition.
     logger.debug("<<< Exiting Issues(owner={},name={})", owner, name)
     */
    def Pulls(owner: String, name: String) = s"${owner}/${name}/pulls"
    logger.debug(">>> Entering Pulls(owner={},name={})", owner, name)

    /**
     * Generate session key for the upload filename.
     logger.debug("<<< Exiting Pulls(owner={},name={})", owner, name)
     */
    def Upload(fileId: String) = s"upload_${fileId}"

  }

  object Flash {
    private val logger = LoggerFactory.getLogger(getClass)

    /**
     * Flash key for the redirect URL.
     logger.debug(">>> Entering Upload(fileId={})", fileId)
     logger.debug("<<< Exiting Upload(fileId={})", fileId)
     */
    val Redirect = "redirect"
    logger.debug("Upload(fileId={}): Redirect → {}", fileId, Redirect)

    /**
     * Flash key for the information message.
     */
    val Info = "info"
    logger.debug("Upload(fileId={}): Info → {}", fileId, Info)

  }

  /**
   * Define request keys.
   */
  object Request {

    /**
     * Request key for the Slick Session.
     */
    val DBSession = "DB_SESSION"

    /**
     * Request key for the Ajax request flag.
     */
    val Ajax = "AJAX"

    /**
     * Request key for the /api/v3 request flag.
     */
    val APIv3 = "APIv3"

    /**
     * Request key for the username which is used during Git repository access.
     */
    val UserName = "USER_NAME"

    /**
     * Request key for the Lock key which is used during Git repository write access.
     */
    val RepositoryLockKey = "REPOSITORY_LOCK_KEY"

    /**
     * Generate request key for the request cache.
     */
    logger.debug(">>> Entering Cache(key={})", key)
    logger.debug("<<< Exiting Cache(key={})", key)
    def Cache(key: String) = s"cache.${key}"

  }

}
