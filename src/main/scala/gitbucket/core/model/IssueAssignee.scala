package gitbucket.core.model

trait IssueAssigneeComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import self._

  lazy val IssueAssignees = TableQuery[IssueAssignees]

  class IssueAssignees(tag: Tag) extends Table[IssueAssignee](tag, "ISSUE_ASSIGNEE") with IssueTemplate {
    private val logger = LoggerFactory.getLogger(getClass)
    val assigneeUserName = column[String]("ASSIGNEE_USER_NAME")
    def * =
      (userName, repositoryName, issueId, assigneeUserName).mapTo[IssueAssignee]

    def byPrimaryKey(owner: String, repository: String, issueId: Int, assigneeUserName: String) = {
      logger.debug(">>> Entering byPrimaryKey(owner={},repository={},issueId={},assigneeUserName={})", owner, repository, issueId, assigneeUserName)
      logger.debug("<<< Exiting byPrimaryKey(owner={},repository={},issueId={},assigneeUserName={})", owner, repository, issueId, assigneeUserName)
      byIssue(owner, repository, issueId) && this.assigneeUserName === assigneeUserName.bind
    }
  }
}

case class IssueAssignee(
  userName: String,
  repositoryName: String,
  issueId: Int,
  assigneeUserName: String
)
