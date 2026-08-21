package gitbucket.core.service

import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, Issue}
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.activity.CreateIssueInfo
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.util.Implicits._

trait IssueCreationService {

  self: RepositoryService & WebHookIssueCommentService & LabelsService & IssuesService & ActivityService =>

  def createIssue(
    repository: RepositoryInfo,
    title: String,
    body: Option[String],
    assignees: Seq[String],
    milestoneId: Option[Int],
    priorityId: Option[Int],
    labelNames: Seq[String],
    loginAccount: Account
  )(implicit context: Context, s: Session): Issue = {
    private val logger = LoggerFactory.getLogger(getClass)

    logger.debug(">>> Entering createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={})", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount)
    val owner = repository.owner
    val name = repository.name
    logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): name → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, name)
    val userName = loginAccount.userName
    logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): userName → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, userName)
    val manageable = isIssueManageable(repository)

    logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): manageable → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, manageable)
    // insert issue
    val issueId = insertIssue(
      owner,
      name,
      userName,
      title,
      body,
      if (manageable) milestoneId else None,
      if (manageable) priorityId else None
    )
    val issue: Issue = getIssue(owner, name, issueId.toString).get
    logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): issue → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, issue)

    if (manageable) {
      // insert assignees
      assignees.foreach { assignee =>
        registerIssueAssignee(owner, name, issueId, assignee)
      }
      // insert labels
      val labels = getLabels(owner, name)
      labelNames.foreach { labelName =>
        labels.find(_.labelName == labelName).map { label =>
          registerIssueLabel(owner, name, issueId, label.labelId)
        logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): labels → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, labels)
        }
      }
    }

    // record activity
    val createIssueInfo = CreateIssueInfo(owner, name, userName, issueId, title)
    recordActivity(createIssueInfo)

    logger.debug("createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={}): createIssueInfo → {}", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount, createIssueInfo)
    // extract references and create refer comment
    createReferComment(owner, name, issue, title + " " + body.getOrElse(""), loginAccount)

    // call web hooks
    callIssuesWebHook("opened", repository, issue, loginAccount, context.settings)

    // call hooks
    PluginRegistry().getIssueHooks.foreach(_.created(issue, repository))

    logger.debug("<<< Exiting createIssue(repository={},title={},body={},assignees={},milestoneId={},priorityId={},labelNames={},loginAccount={})", repository, title, body, assignees, milestoneId, priorityId, labelNames, loginAccount)
    issue
  }

  /**
   * Tests whether an logged-in user can manage issues.
   */
  protected def isIssueManageable(repository: RepositoryInfo)(implicit context: Context, s: Session): Boolean = {
    logger.debug(">>> Entering isIssueManageable(repository={})", repository)
    logger.debug("<<< Exiting isIssueManageable(repository={})", repository)
    hasDeveloperRole(repository.owner, repository.name, context.loginAccount)
  }

  /**
   * Tests whether an logged-in user can manage issues comment.
   */
  protected def isIssueCommentManageable(repository: RepositoryInfo)(implicit context: Context, s: Session): Boolean = {
    logger.debug(">>> Entering isIssueCommentManageable(repository={})", repository)
    logger.debug("<<< Exiting isIssueCommentManageable(repository={})", repository)
    hasOwnerRole(repository.owner, repository.name, context.loginAccount)
  }

  /**
   * Tests whether an logged-in user can post issues.
   */
  protected def isIssueEditable(repository: RepositoryInfo)(implicit context: Context, s: Session): Boolean = {
    logger.debug(">>> Entering isIssueEditable(repository={})", repository)
    repository.repository.options.issuesOption match {
      case "ALL"     =>
        logger.debug("<<< Exiting isIssueEditable(repository={})", repository)
        !repository.repository.isPrivate && context.loginAccount.isDefined
      case "PUBLIC"  => hasGuestRole(repository.owner, repository.name, context.loginAccount)
      case "PRIVATE" => hasDeveloperRole(repository.owner, repository.name, context.loginAccount)
      case "DISABLE" => false
    }
  }
}
