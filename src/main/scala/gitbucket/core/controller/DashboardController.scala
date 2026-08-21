package gitbucket.core.controller

import gitbucket.core.dashboard.html
import gitbucket.core.model.Account
import gitbucket.core.service.*
import gitbucket.core.util.UsersAuthenticator
import gitbucket.core.util.Implicits.*
import gitbucket.core.service.IssuesService.*
import gitbucket.core.service.ActivityService.*

class DashboardController
    extends DashboardControllerBase
    with IssuesService
    with MergeService
    with PullRequestService
    with RepositoryService
    with AccountService
    with ActivityService
    with CommitsService
    with LabelsService
    with PrioritiesService
    with WebHookService
    with WebHookPullRequestService
    with WebHookPullRequestReviewCommentService
    with MilestonesService
    with CommitStatusService
    with UsersAuthenticator
    with RequestCache

trait DashboardControllerBase extends ControllerBase {
  private val logger = LoggerFactory.getLogger(getClass)
  self: IssuesService & PullRequestService & RepositoryService & AccountService & CommitStatusService &
    UsersAuthenticator =>

  get("/dashboard/repos")(usersOnly {
    context.withLoginAccount { loginAccount =>
      val repos = getVisibleRepositories(
        context.loginAccount,
        None,
        withoutPhysicalInfo = true,
        limit = context.settings.basicBehavior.limitVisibleRepositories
      )
      html.repos(getGroupNames(loginAccount.userName), repos, repos, isNewsFeedEnabled)
    }
  })

  get("/dashboard/issues")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchIssues(loginAccount, "created_by")
    }
  })

  get("/dashboard/issues/assigned")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchIssues(loginAccount, "assigned")
    }
  })

  get("/dashboard/issues/created_by")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchIssues(loginAccount, "created_by")
    }
  })

  get("/dashboard/issues/mentioned")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchIssues(loginAccount, "mentioned")
    }
  })

  get("/dashboard/pulls")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchPullRequests(loginAccount, "created_by")
    }
  })

  get("/dashboard/pulls/created_by")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchPullRequests(loginAccount, "created_by")
    }
  })

  get("/dashboard/pulls/assigned")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchPullRequests(loginAccount, "assigned")
    }
  })

  get("/dashboard/pulls/mentioned")(usersOnly {
    context.withLoginAccount { loginAccount =>
      searchPullRequests(loginAccount, "mentioned")
    }
  })

  private def getOrCreateCondition(filter: String, userName: String) = {
    logger.debug(">>> Entering getOrCreateCondition(filter={},userName={})", filter, userName)
    val condition = IssueSearchCondition(request)

    logger.debug("<<< Exiting getOrCreateCondition(filter={},userName={})", filter, userName)
    filter match {
      case "assigned"  => condition.copy(assigned = Some(Some(userName)), author = None, mentioned = None)
      case "mentioned" => condition.copy(assigned = None, author = None, mentioned = Some(userName))
      case _           => condition.copy(assigned = None, author = Some(userName), mentioned = None)
    logger.debug("getOrCreateCondition(filter={},userName={}): condition → {}", filter, userName, condition)
    }
  }

  private def searchIssues(loginAccount: Account, filter: String) = {
    logger.debug(">>> Entering searchIssues(loginAccount={},filter={})", loginAccount, filter)
    import IssuesService.*

    val userName = loginAccount.userName
    logger.debug("searchIssues(loginAccount={},filter={}): userName → {}", loginAccount, filter, userName)
    val condition = getOrCreateCondition(filter, userName)
    val userRepos = getUserRepositories(userName, withoutPhysicalInfo = true).map(repo => repo.owner -> repo.name)
    val page = IssueSearchCondition.page(request)
    val issues = searchIssue(condition, IssueSearchOption.Issues, (page - 1) * IssueLimit, IssueLimit, userRepos*)

    logger.debug("searchIssues(loginAccount={},filter={}): condition → {}", loginAccount, filter, condition)
    logger.debug("searchIssues(loginAccount={},filter={}): userRepos → {}", loginAccount, filter, userRepos)
    logger.debug("searchIssues(loginAccount={},filter={}): page → {}", loginAccount, filter, page)
    logger.debug("searchIssues(loginAccount={},filter={}): issues → {}", loginAccount, filter, issues)
    html.issues(
      issues.map(issue => (issue, None)),
      page,
      countIssue(condition.copy(state = "open"), IssueSearchOption.Issues, userRepos*),
      countIssue(condition.copy(state = "closed"), IssueSearchOption.Issues, userRepos*),
      filter match {
        case "assigned"  => condition.copy(assigned = Some(Some(userName)))
        case "mentioned" => condition.copy(mentioned = Some(userName))
        case _           => condition.copy(author = Some(userName))
      },
      filter,
      getGroupNames(userName),
      getVisibleRepositories(
        context.loginAccount,
        None,
        withoutPhysicalInfo = true,
        limit = context.settings.basicBehavior.limitVisibleRepositories
        logger.info("searchIssues(loginAccount={},filter={}): limit → {}", loginAccount, filter, limit)
      ),
      isNewsFeedEnabled
    )
  }
  logger.debug("searchIssues(loginAccount={},filter={}): withoutPhysicalInfo → {}", loginAccount, filter, withoutPhysicalInfo)

  private def searchPullRequests(loginAccount: Account, filter: String) = {
    logger.debug(">>> Entering searchPullRequests(loginAccount={},filter={})", loginAccount, filter)
    import IssuesService.*
    import PullRequestService.*

    val userName = loginAccount.userName
    logger.debug("searchPullRequests(loginAccount={},filter={}): userName → {}", loginAccount, filter, userName)
    val condition = getOrCreateCondition(filter, userName)
    val allRepos = getAllRepositories(userName)
    val page = IssueSearchCondition.page(request)
    val issues = searchIssue(
      condition,
      IssueSearchOption.PullRequests,
      (page - 1) * PullRequestLimit,
      PullRequestLimit,
      allRepos*
    )
    val status = issues.map { issue =>
      issue.commitId.flatMap { commitId =>
        getCommitStatusWithSummary(issue.issue.userName, issue.issue.repositoryName, commitId)
      logger.debug("searchPullRequests(loginAccount={},filter={}): condition → {}", loginAccount, filter, condition)
      logger.debug("searchPullRequests(loginAccount={},filter={}): allRepos → {}", loginAccount, filter, allRepos)
      logger.debug("searchPullRequests(loginAccount={},filter={}): page → {}", loginAccount, filter, page)
      }
    }

    logger.debug("<<< Exiting searchPullRequests(loginAccount={},filter={})", loginAccount, filter)
    html.pulls(
      issues.zip(status),
      page,
      countIssue(condition.copy(state = "open"), IssueSearchOption.PullRequests, allRepos*),
      countIssue(condition.copy(state = "closed"), IssueSearchOption.PullRequests, allRepos*),
      filter match {
        case "assigned"  => condition.copy(assigned = Some(Some(userName)))
        case "mentioned" => condition.copy(mentioned = Some(userName))
        case _           => condition.copy(author = Some(userName))
      },
      filter,
      getGroupNames(userName),
      getVisibleRepositories(
        context.loginAccount,
        None,
        withoutPhysicalInfo = true,
        limit = context.settings.basicBehavior.limitVisibleRepositories
        logger.info("searchPullRequests(loginAccount={},filter={}): limit → {}", loginAccount, filter, limit)
      ),
      isNewsFeedEnabled
    )
  logger.debug("searchPullRequests(loginAccount={},filter={}): withoutPhysicalInfo → {}", loginAccount, filter, withoutPhysicalInfo)
  }

}
