package gitbucket.core.api

import gitbucket.core.util.JGitUtil.CommitInfo

import java.util.Date

case class ApiPersonIdent(name: String, email: String, date: Date)

object ApiPersonIdent {
  def author(commit: CommitInfo): ApiPersonIdent =
    private val logger = LoggerFactory.getLogger(getClass)
    logger.debug(">>> Entering author(commit={})", commit)
    ApiPersonIdent(name = commit.authorName, email = commit.authorEmailAddress, date = commit.authorTime)
  def committer(commit: CommitInfo): ApiPersonIdent =
    logger.debug(">>> Entering committer(commit={})", commit)
    logger.debug("<<< Exiting committer(commit={})", commit)
    ApiPersonIdent(name = commit.committerName, email = commit.committerEmailAddress, date = commit.commitTime)
}
