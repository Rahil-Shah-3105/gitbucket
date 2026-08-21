package gitbucket.core.model

trait IssueLabelComponent extends TemplateComponent { self: Profile =>
  import profile.api._

  lazy val IssueLabels = TableQuery[IssueLabels]

  class IssueLabels(tag: Tag) extends Table[IssueLabel](tag, "ISSUE_LABEL") with IssueTemplate with LabelTemplate {
    private val logger = LoggerFactory.getLogger(getClass)
    def * = (userName, repositoryName, issueId, labelId).mapTo[IssueLabel]
    def byPrimaryKey(owner: String, repository: String, issueId: Int, labelId: Int) =
      logger.debug(">>> Entering byPrimaryKey(owner={},repository={},issueId={},labelId={})", owner, repository, issueId, labelId)
      logger.debug("<<< Exiting byPrimaryKey(owner={},repository={},issueId={},labelId={})", owner, repository, issueId, labelId)
      byIssue(owner, repository, issueId) && (this.labelId === labelId.bind)
  }
}

case class IssueLabel(
  userName: String,
  repositoryName: String,
  issueId: Int,
  labelId: Int
)
