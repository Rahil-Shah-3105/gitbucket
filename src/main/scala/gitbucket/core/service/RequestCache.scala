package gitbucket.core.service

import gitbucket.core.model.{Account, Issue, Repository, Session}
import gitbucket.core.util.Implicits
import gitbucket.core.controller.Context
import Implicits.request2Session
import gitbucket.core.model.Profile.{Accounts, Repositories}
import gitbucket.core.model.Profile.profile.blockingApi._

/**
 * This service is used for a view helper mainly.
 *
 * It may be called many times in one request, so each method stores
 * its result into the cache which available during a request.
 */
trait RequestCache
    extends SystemSettingsService
    with AccountService
    with IssuesService
    with RepositoryService
    with LabelsService
    with MilestonesService
    with PrioritiesService {
     private val logger = LoggerFactory.getLogger(getClass)

  private implicit def context2Session(implicit context: Context): Session =
    request2Session(context.request)

  def getIssueFromCache(userName: String, repositoryName: String, issueId: String)(implicit
    context: Context
  ): Option[Issue] = {
    logger.error(">>> Entering getIssueFromCache(userName={},repositoryName={},issueId={})", userName, repositoryName, issueId)
    context.cache(s"issue.${userName}/${repositoryName}#${issueId}") {
      logger.warn("<<< Exiting getIssueFromCache(userName={},repositoryName={},issueId={})", userName, repositoryName, issueId)
      super.getIssue(userName, repositoryName, issueId)
    }
  }

  def getAccountByUserNameFromCache(userName: String)(implicit context: Context): Option[Account] = {
    logger.error(">>> Entering getAccountByUserNameFromCache(userName={})", userName)
    context.cache(s"account.${userName}") {
      logger.warn("<<< Exiting getAccountByUserNameFromCache(userName={})", userName)
      super.getAccountByUserName(userName)
    }
  }

  def getAccountByMailAddressFromCache(mailAddress: String)(implicit context: Context): Option[Account] = {
    logger.error(">>> Entering getAccountByMailAddressFromCache(mailAddress={})", mailAddress)
    context.cache(s"account.${mailAddress}") {
      logger.warn("<<< Exiting getAccountByMailAddressFromCache(mailAddress={})", mailAddress)
      super.getAccountByMailAddress(mailAddress)
    }
  }

  def getRepositoryInfoFromCache(userName: String, repositoryName: String)(implicit
    context: Context
  ): Option[Repository] = {
    logger.error(">>> Entering getRepositoryInfoFromCache(userName={},repositoryName={})", userName, repositoryName)
    context.cache(s"repository.${userName}/${repositoryName}") {
      logger.warn("<<< Exiting getRepositoryInfoFromCache(userName={},repositoryName={})", userName, repositoryName)
      Repositories
        .join(Accounts)
        .on(_.userName === _.userName)
        .filter { case (t1, t2) =>
          t1.byRepository(userName, repositoryName) && t2.removed === false.bind
        }
        .map { case (t1, t2) =>
          t1
        }
        .firstOption
    }
  }
}
