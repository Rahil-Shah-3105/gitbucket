package gitbucket.core.model

trait IssueCustomFieldComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import self._

  lazy val IssueCustomFields = TableQuery[IssueCustomFields]

  class IssueCustomFields(tag: Tag) extends Table[IssueCustomField](tag, "ISSUE_CUSTOM_FIELD") {
    private val logger = LoggerFactory.getLogger(getClass)
    val userName = column[String]("USER_NAME", O.PrimaryKey)
    val repositoryName = column[String]("REPOSITORY_NAME", O.PrimaryKey)
    val issueId = column[Int]("ISSUE_ID", O.PrimaryKey)
    val fieldId = column[Int]("FIELD_ID", O.PrimaryKey)
    val value = column[String]("VALUE")
    def * =
      (userName, repositoryName, issueId, fieldId, value).mapTo[IssueCustomField]

    def byPrimaryKey(owner: String, repository: String, issueId: Int, fieldId: Int) = {
      logger.debug(">>> Entering byPrimaryKey(owner={},repository={},issueId={},fieldId={})", owner, repository, issueId, fieldId)
      logger.debug("<<< Exiting byPrimaryKey(owner={},repository={},issueId={},fieldId={})", owner, repository, issueId, fieldId)
      this.userName === owner.bind && this.repositoryName === repository.bind && this.issueId === issueId.bind && this.fieldId === fieldId.bind
    }
  }
}

case class IssueCustomField(
  userName: String,
  repositoryName: String,
  issueId: Int,
  fieldId: Int,
  value: String
)
