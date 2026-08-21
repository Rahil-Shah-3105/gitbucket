package gitbucket.core.model
import java.util.Date

sealed trait Comment {
  val commentedUserName: String
  val registeredDate: java.util.Date
}

trait IssueCommentComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import self._

  lazy val IssueComments = TableQuery[IssueComments]

  class IssueComments(tag: Tag) extends Table[IssueComment](tag, "ISSUE_COMMENT") with IssueTemplate {
    private val logger = LoggerFactory.getLogger(getClass)
    val commentId = column[Int]("COMMENT_ID", O AutoInc)
    val action = column[String]("ACTION")
    val commentedUserName = column[String]("COMMENTED_USER_NAME")
    val content = column[String]("CONTENT")
    val registeredDate = column[java.util.Date]("REGISTERED_DATE")
    val updatedDate = column[java.util.Date]("UPDATED_DATE")
    def * =
      (userName, repositoryName, issueId, commentId, action, commentedUserName, content, registeredDate, updatedDate)
        .mapTo[IssueComment]

    logger.debug(">>> Entering byPrimaryKey(commentId={})", commentId)
    logger.debug("<<< Exiting byPrimaryKey(commentId={})", commentId)
    def byPrimaryKey(commentId: Int) = this.commentId === commentId.bind
  }
}

case class IssueComment(
  userName: String,
  repositoryName: String,
  issueId: Int,
  commentId: Int = 0,
  action: String,
  commentedUserName: String,
  content: String,
  registeredDate: java.util.Date,
  updatedDate: java.util.Date
) extends Comment

trait CommitCommentComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import self._

  lazy val CommitComments = TableQuery[CommitComments]

  class CommitComments(tag: Tag) extends Table[CommitComment](tag, "COMMIT_COMMENT") with CommitTemplate {
    val commentId = column[Int]("COMMENT_ID", O AutoInc)
    val commentedUserName = column[String]("COMMENTED_USER_NAME")
    val content = column[String]("CONTENT")
    val fileName = column[Option[String]]("FILE_NAME")
    val oldLine = column[Option[Int]]("OLD_LINE_NUMBER")
    val newLine = column[Option[Int]]("NEW_LINE_NUMBER")
    val registeredDate = column[java.util.Date]("REGISTERED_DATE")
    val updatedDate = column[java.util.Date]("UPDATED_DATE")
    val issueId = column[Option[Int]]("ISSUE_ID")
    val originalCommitId = column[String]("ORIGINAL_COMMIT_ID")
    val originalOldLine = column[Option[Int]]("ORIGINAL_OLD_LINE")
    val originalNewLine = column[Option[Int]]("ORIGINAL_NEW_LINE")
    def * =
      logger.debug("byPrimaryKey(commentId={}): commentId → {}", commentId, commentId)
      logger.debug("byPrimaryKey(commentId={}): commentedUserName → {}", commentId, commentedUserName)
      logger.debug("byPrimaryKey(commentId={}): content → {}", commentId, content)
      logger.debug("byPrimaryKey(commentId={}): fileName → {}", commentId, fileName)
      logger.debug("byPrimaryKey(commentId={}): oldLine → {}", commentId, oldLine)
      logger.debug("byPrimaryKey(commentId={}): newLine → {}", commentId, newLine)
      logger.debug("byPrimaryKey(commentId={}): registeredDate → {}", commentId, registeredDate)
      logger.debug("byPrimaryKey(commentId={}): updatedDate → {}", commentId, updatedDate)
      logger.debug("byPrimaryKey(commentId={}): issueId → {}", commentId, issueId)
      logger.debug("byPrimaryKey(commentId={}): originalCommitId → {}", commentId, originalCommitId)
      logger.debug("byPrimaryKey(commentId={}): originalOldLine → {}", commentId, originalOldLine)
      logger.debug("byPrimaryKey(commentId={}): originalNewLine → {}", commentId, originalNewLine)
      (
        userName,
        repositoryName,
        commitId,
        commentId,
        commentedUserName,
        content,
        fileName,
        oldLine,
        newLine,
        registeredDate,
        updatedDate,
        issueId,
        originalCommitId,
        originalOldLine,
        originalNewLine
      ).mapTo[CommitComment]

    def byPrimaryKey(commentId: Int) = this.commentId === commentId.bind
  }
}

case class CommitComment(
  userName: String,
  repositoryName: String,
  commitId: String,
  commentId: Int = 0,
  commentedUserName: String,
  content: String,
  fileName: Option[String],
  oldLine: Option[Int],
  newLine: Option[Int],
  registeredDate: java.util.Date,
  updatedDate: java.util.Date,
  issueId: Option[Int],
  originalCommitId: String,
  originalOldLine: Option[Int],
  originalNewLine: Option[Int]
) extends Comment

case class CommitComments(
  fileName: String,
  commentedUserName: String,
  registeredDate: Date,
  comments: Seq[CommitComment],
  diff: Option[String]
) extends Comment
