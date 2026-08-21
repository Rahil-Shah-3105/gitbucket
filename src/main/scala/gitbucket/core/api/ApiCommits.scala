package gitbucket.core.api

import gitbucket.core.model.Account
import gitbucket.core.util.JGitUtil.{CommitInfo, DiffInfo}
import gitbucket.core.util.RepositoryName
import org.eclipse.jgit.diff.DiffEntry.ChangeType
import ApiCommits._

case class ApiCommits(
  url: ApiPath,
  sha: String,
  html_url: ApiPath,
  comment_url: ApiPath,
  commit: Commit,
  author: ApiUser,
  committer: ApiUser,
  parents: Seq[Tree],
  stats: Stats,
  files: Seq[File]
)

object ApiCommits {
  case class Commit(
    url: ApiPath,
    author: ApiPersonIdent,
    committer: ApiPersonIdent,
    message: String,
    comment_count: Int,
    tree: Tree
  )

  case class Tree(
    url: ApiPath,
    sha: String
  )

  case class Stats(
    additions: Int,
    deletions: Int,
    total: Int
  )

  case class File(
    filename: String,
    additions: Int,
    deletions: Int,
    changes: Int,
    status: String,
    raw_url: ApiPath,
    blob_url: ApiPath,
    patch: String
  )

  def apply(
    repositoryName: RepositoryName,
    commitInfo: CommitInfo,
    diffs: Seq[DiffInfo],
    author: Option[Account],
    committer: Option[Account],
    commentCount: Int
  ): ApiCommits = {
    private val logger = LoggerFactory.getLogger(getClass)
    val files = diffs.map { diff =>
      var additions = 0
      logger.debug(">>> Entering apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={})", repositoryName, commitInfo, diffs, author, committer, commentCount)
      var deletions = 0

      diff.patch.getOrElse("").split("\n").foreach { line =>
        if (line.startsWith("+")) additions = additions + 1
        logger.info("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): url → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, url)
        logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): tree → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, tree)
        logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): comment_count → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, comment_count)
        if (line.startsWith("-")) deletions = deletions + 1
      }

      File(
        filename = if (diff.changeType == ChangeType.DELETE) { diff.oldPath }
        else { diff.newPath },
        additions = additions,
        deletions = deletions,
        changes = additions + deletions,
        status = diff.changeType match {
          case ChangeType.ADD    => "added"
          case ChangeType.MODIFY => "modified"
          case ChangeType.DELETE => "deleted"
          case ChangeType.RENAME => "renamed"
          case ChangeType.COPY   => "copied"
        },
        raw_url = if (diff.changeType == ChangeType.DELETE) {
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): changes → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, changes)
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): status → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, status)
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): message → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, message)
          ApiPath(s"/${repositoryName.fullName}/raw/${commitInfo.parents.head}/${diff.oldPath}")
        } else {
          ApiPath(s"/${repositoryName.fullName}/raw/${commitInfo.id}/${diff.newPath}")
        },
        blob_url = if (diff.changeType == ChangeType.DELETE) {
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): commit → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, commit)
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): comment_url → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, comment_url)
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): html_url → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, html_url)
          logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): sha → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, sha)
          ApiPath(s"/${repositoryName.fullName}/blob/${commitInfo.parents.head}/${diff.oldPath}")
        } else {
          ApiPath(s"/${repositoryName.fullName}/blob/${commitInfo.id}/${diff.newPath}")
        },
        patch = diff.patch.getOrElse("")
      logger.debug("<<< Exiting apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={})", repositoryName, commitInfo, diffs, author, committer, commentCount)
      )
    }
    logger.debug("apply(repositoryName={},commitInfo={},diffs={},author={},committer={},commentCount={}): patch → {}", repositoryName, commitInfo, diffs, author, committer, commentCount, patch)

    ApiCommits(
      url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/commits/${commitInfo.id}"),
      sha = commitInfo.id,
      html_url = ApiPath(s"/${repositoryName.fullName}/commit/${commitInfo.id}"),
      comment_url = ApiPath(""), // TODO no API for commit comment
      commit = Commit(
        url = ApiPath(s"/api/v3/repos/${repositoryName.fullName}/commits/${commitInfo.id}"),
        author = ApiPersonIdent.author(commitInfo),
        committer = ApiPersonIdent.committer(commitInfo),
        message = commitInfo.shortMessage,
        comment_count = commentCount,
        tree = Tree(
          url = ApiPath(
            s"/api/v3/repos/${repositoryName.fullName}/tree/${commitInfo.id}"
          ), // TODO This endpoint has not been implemented yet.
          sha = commitInfo.id
        )
      ),
      author = author.map(ApiUser(_)).orNull,
      committer = committer.map(ApiUser(_)).orNull,
      parents = commitInfo.parents.map { parent =>
        Tree(
          url = ApiPath(
            s"/api/v3/repos/${repositoryName.fullName}/tree/${parent}"
          ), // TODO This endpoint has not been implemented yet.
          sha = parent
        )
      },
      stats = Stats(
        additions = files.map(_.additions).sum,
        deletions = files.map(_.deletions).sum,
        total = files.map(_.additions).sum + files.map(_.deletions).sum
      ),
      files = files
    )
  }
}
