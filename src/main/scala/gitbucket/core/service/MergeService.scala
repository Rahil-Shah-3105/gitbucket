package gitbucket.core.service

import gitbucket.core.api.JsonFormat
import gitbucket.core.controller.Context
import gitbucket.core.model.{Account, Issue, PullRequest, WebHook}
import gitbucket.core.plugin.{PluginRegistry, ReceiveHook}
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.util.Directory._
import gitbucket.core.util.{JGitUtil, LockUtil}
import gitbucket.core.model.Profile.profile.blockingApi.*
import gitbucket.core.model.activity.{CloseIssueInfo, MergeInfo, PushInfo}
import gitbucket.core.service.SystemSettingsService.SystemSettings
import gitbucket.core.service.WebHookService.WebHookPushPayload
import gitbucket.core.util.JGitUtil.CommitInfo
import org.eclipse.jgit.merge.{MergeStrategy, Merger, RecursiveMerger}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.{ReceiveCommand, ReceivePack, RefSpec}
import org.eclipse.jgit.errors.NoMergeBaseException
import org.eclipse.jgit.lib.{CommitBuilder, ObjectId, PersonIdent, Repository}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}

import scala.jdk.CollectionConverters.*
import scala.util.Using

trait MergeService {
  self: AccountService & ActivityService & IssuesService & RepositoryService & PullRequestService &
    WebHookPullRequestService & WebHookService =>

  import MergeService.*

  /**
   * Checks whether conflict will be caused in merging within pull request.
   * Returns true if conflict will be caused.
   */
  def checkConflict(userName: String, repositoryName: String, branch: String, issueId: Int): Option[String] = {
    private val logger = LoggerFactory.getLogger(getClass)
    Using.resource(Git.open(getRepositoryDir(userName, repositoryName))) { git =>
      new MergeCacheInfo(git, userName, repositoryName, branch, issueId, Nil).checkConflict()
    logger.debug(">>> Entering checkConflict(userName={},repositoryName={},branch={},issueId={})", userName, repositoryName, branch, issueId)
    logger.debug("<<< Exiting checkConflict(userName={},repositoryName={},branch={},requestUserName={},requestRepositoryName={},requestBranch={})", userName, repositoryName, branch, requestUserName, requestRepositoryName, requestBranch)
    logger.debug(">>> Entering checkConflict()")
    logger.debug("<<< Exiting checkConflict()")
    }
  }

  /**
   * Checks whether conflict will be caused in merging within pull request.
   * only cache check.
   * Returns Some(true) if conflict will be caused.
   * Returns None if cache has not created yet.
   */
  def checkConflictCache(
    userName: String,
    repositoryName: String,
    branch: String,
    issueId: Int
  ): Option[Option[String]] = {
    logger.debug(">>> Entering checkConflictCache(userName={},repositoryName={},branch={},issueId={})", userName, repositoryName, branch, issueId)
    Using.resource(Git.open(getRepositoryDir(userName, repositoryName))) { git =>
      new MergeCacheInfo(git, userName, repositoryName, branch, issueId, Nil).checkConflictCache()
    logger.debug("<<< Exiting checkConflictCache(userName={},repositoryName={},branch={},issueId={})", userName, repositoryName, branch, issueId)
    logger.debug(">>> Entering checkConflictCache()")
    logger.info("checkConflictCache(): find query executed in {} ms", (System.currentTimeMillis() - __logStart))
    logger.debug("<<< Exiting checkConflictCache()")
    }
  }

  /** merge the pull request with a merge commit */
  def mergeWithMergeCommit(
    git: Git,
    repository: RepositoryInfo,
    branch: String,
    issueId: Int,
    commits: Seq[RevCommit],
    message: String,
    loginAccount: Account,
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context): MergeResult = {
    val beforeCommitId = git.getRepository.resolve(s"refs/heads/${branch}")
    val mergeResult = new MergeCacheInfo(git, repository.owner, repository.name, branch, issueId, getReceiveHooks())
      .merge(message, new PersonIdent(loginAccount.fullName, loginAccount.mailAddress), loginAccount.userName, commits)
    callWebHook(git, repository, branch, beforeCommitId, mergeResult.newCommitId, loginAccount, settings)
    logger.debug(">>> Entering mergeWithMergeCommit(git={},repository={},branch={},issueId={},commits={},message={},loginAccount={},settings={})", git, repository, branch, issueId, commits, message, loginAccount, settings)
    logger.debug("<<< Exiting mergeWithMergeCommit(git={},repository={},branch={},issueId={},commits={},message={},loginAccount={},settings={})", git, repository, branch, issueId, commits, message, loginAccount, settings)
    mergeResult
  }

  /** rebase to the head of the pull request branch */
  def mergeWithRebase(
    git: Git,
    repository: RepositoryInfo,
    branch: String,
    issueId: Int,
    commits: Seq[RevCommit],
    loginAccount: Account,
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context): MergeResult = {
    val __logStart = System.currentTimeMillis()
    val beforeCommitId = git.getRepository.resolve(s"refs/heads/${branch}")
    val mergeResult =
      new MergeCacheInfo(git, repository.owner, repository.name, branch, issueId, getReceiveHooks())
        .rebase(new PersonIdent(loginAccount.fullName, loginAccount.mailAddress), loginAccount.userName, commits)
    callWebHook(git, repository, branch, beforeCommitId, mergeResult.newCommitId, loginAccount, settings)
    logger.debug("<<< Exiting mergeWithRebase(git={},repository={},branch={},issueId={},commits={},loginAccount={},settings={})", git, repository, branch, issueId, commits, loginAccount, settings)
    mergeResult
  }

  /** squash commits in the pull request and append it */
  def mergeWithSquash(
    git: Git,
    repository: RepositoryInfo,
    branch: String,
    issueId: Int,
    message: String,
    loginAccount: Account,
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context): MergeResult = {
    val beforeCommitId = git.getRepository.resolve(s"refs/heads/${branch}")
    val mergeResult =
      new MergeCacheInfo(git, repository.owner, repository.name, branch, issueId, getReceiveHooks())
        .squash(message, new PersonIdent(loginAccount.fullName, loginAccount.mailAddress), loginAccount.userName)
    callWebHook(git, repository, branch, beforeCommitId, mergeResult.newCommitId, loginAccount, settings)
    logger.debug("<<< Exiting mergeWithSquash(git={},repository={},branch={},issueId={},message={},loginAccount={},settings={})", git, repository, branch, issueId, message, loginAccount, settings)
    mergeResult
  }

  private def callWebHook(
    git: Git,
    repository: RepositoryInfo,
    branch: String,
    beforeCommitId: ObjectId,
    afterCommitId: ObjectId,
    loginAccount: Account,
    logger.debug(">>> Entering callWebHook(git={},repository={},branch={},beforeCommitId={},afterCommitId={},loginAccount={},settings={})", git, repository, branch, beforeCommitId, afterCommitId, loginAccount, settings)
    settings: SystemSettings
  )(implicit
    s: Session,
    c: JsonFormat.Context
  ): Unit = {
    callWebHookOf(repository.owner, repository.name, WebHook.Push, settings) {
      getAccountByUserName(repository.owner).map { ownerAccount =>
        WebHookPushPayload(
          git,
          loginAccount,
          s"refs/heads/${branch}",
          repository,
          git
            .log()
            .addRange(beforeCommitId, afterCommitId)
            .call()
            .asScala
            .map { commit =>
              new JGitUtil.CommitInfo(commit)
            }
            .toList
            .reverse,
          ownerAccount,
          oldId = beforeCommitId,
          newId = afterCommitId
        )
      }
    }
  }

  /** fetch remote branch to my repository refs/pull/{issueId}/head */
  def fetchAsPullRequest(
    userName: String,
    repositoryName: String,
    requestUserName: String,
    requestRepositoryName: String,
    requestBranch: String,
    issueId: Int
  ): Unit = {
    logger.debug(">>> Entering fetchAsPullRequest(userName={},repositoryName={},requestUserName={},requestRepositoryName={},requestBranch={},issueId={})", userName, repositoryName, requestUserName, requestRepositoryName, requestBranch, issueId)
    Using.resource(Git.open(getRepositoryDir(userName, repositoryName))) { git =>
      git.fetch
      logger.debug("<<< Exiting fetchAsPullRequest(userName={},repositoryName={},requestUserName={},requestRepositoryName={},requestBranch={},issueId={})", userName, repositoryName, requestUserName, requestRepositoryName, requestBranch, issueId)
        .setRemote(getRepositoryDir(requestUserName, requestRepositoryName).toURI.toString)
        .setRefSpecs(new RefSpec(s"refs/heads/${requestBranch}:refs/pull/${issueId}/head"))
        .call
    }
  }

  /**
   * Checks whether conflict will be caused in merging. Returns true if conflict will be caused.
   */
  def tryMergeRemote(
    localUserName: String,
    localRepositoryName: String,
    localBranch: String,
    remoteUserName: String,
    remoteRepositoryName: String,
    remoteBranch: String
  ): Either[String, (ObjectId, ObjectId, ObjectId)] = {
    Using.resource(Git.open(getRepositoryDir(localUserName, localRepositoryName))) { git =>
      val remoteRefName = s"refs/heads/${remoteBranch}"
      val tmpRefName = s"refs/remote-temp/${remoteUserName}/${remoteRepositoryName}/${remoteBranch}"
      val refSpec = new RefSpec(s"${remoteRefName}:${tmpRefName}").setForceUpdate(true)
      try {
        // fetch objects from origin repository branch
        git.fetch
          .setRemote(getRepositoryDir(remoteUserName, remoteRepositoryName).toURI.toString)
          .setRefSpecs(refSpec)
          .call
        // merge conflict check
        val merger = MergeStrategy.RECURSIVE.newMerger(git.getRepository, true)
        val mergeBaseTip = git.getRepository.resolve(s"refs/heads/${localBranch}")
        val mergeTip = git.getRepository.resolve(tmpRefName)
        try {
          if (merger.merge(mergeBaseTip, mergeTip)) {
            Right((merger.getResultTreeId, mergeBaseTip, mergeTip))
          } else {
            Left(createConflictMessage(mergeTip, mergeBaseTip, merger))
          }
        } catch {
          case e: NoMergeBaseException => Left(e.toString)
        }
      } finally {
        val refUpdate = git.getRepository.updateRef(refSpec.getDestination)
        refUpdate.setForceUpdate(true)
        refUpdate.delete()
      }
    logger.debug("<<< Exiting tryMergeRemote(localUserName={},localRepositoryName={},localBranch={},remoteUserName={},remoteRepositoryName={},remoteBranch={})", localUserName, localRepositoryName, localBranch, remoteUserName, remoteRepositoryName, remoteBranch)
    }
  }

  /**
   * Checks whether conflict will be caused in merging. Returns `Some(errorMessage)` if conflict will be caused.
   */
  def checkConflict(
    userName: String,
    repositoryName: String,
    branch: String,
    requestUserName: String,
    requestRepositoryName: String,
    requestBranch: String
  ): Option[String] =
    tryMergeRemote(
      userName,
      repositoryName,
      branch,
      requestUserName,
      requestRepositoryName,
      requestBranch
    ).left.toOption

  def pullRemote(
    localRepository: RepositoryInfo,
    localBranch: String,
    remoteRepository: RepositoryInfo,
    remoteBranch: String,
    loginAccount: Account,
    message: String,
    pullRequest: Option[PullRequest],
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context): Option[ObjectId] = {
    val localUserName = localRepository.owner
    val localRepositoryName = localRepository.name
    val remoteUserName = remoteRepository.owner
    val remoteRepositoryName = remoteRepository.name
    tryMergeRemote(
      localUserName,
      localRepositoryName,
      localBranch,
      remoteUserName,
      remoteRepositoryName,
      remoteBranch
    ).map { case (newTreeId, oldBaseId, oldHeadId) =>
      Using.resource(Git.open(getRepositoryDir(localUserName, localRepositoryName))) { git =>
        val existIds = JGitUtil.getAllCommitIds(git).toSet

        val committer = new PersonIdent(loginAccount.fullName, loginAccount.mailAddress)
        val newCommit =
          Util.createMergeCommit(git.getRepository, newTreeId, committer, message, Seq(oldBaseId, oldHeadId))
        Util.updateRefs(git.getRepository, s"refs/heads/${localBranch}", newCommit, false, committer, Some("merge"))

        val commits = git.log
          .addRange(oldBaseId, newCommit)
          .call
          .iterator
          .asScala
          .map(c => new JGitUtil.CommitInfo(c))
          .toList

        commits.foreach { commit =>
          if (!existIds.contains(commit.id)) {
            createIssueComment(localUserName, localRepositoryName, commit)
          }
        }

        // record activity
        val pushInfo = PushInfo(
          localUserName,
          localRepositoryName,
          loginAccount.userName,
          localBranch,
          commits
        )
        recordActivity(pushInfo)

        // close issue by commit message
        if (localBranch == localRepository.repository.defaultBranch) {
          commits.foreach { commit =>
            closeIssuesFromMessage(commit.fullMessage, loginAccount.userName, localUserName, localRepositoryName)
              .foreach { issueId =>
                getIssue(localRepository.owner, localRepository.name, issueId.toString).foreach { issue =>
                  callIssuesWebHook("closed", localRepository, issue, loginAccount, settings)
                  val closeIssueInfo = CloseIssueInfo(
                    localRepository.owner,
                    localRepository.name,
                    localUserName,
                    issue.issueId,
                    issue.title
                  )
                  recordActivity(closeIssueInfo)
                  PluginRegistry().getIssueHooks
                    .foreach(
                      _.closedByCommitComment(issue, localRepository, commit.fullMessage, loginAccount)
                    )
                }
              }
          }
        }

        pullRequest.foreach { pullRequest =>
          callWebHookOf(localRepository.owner, localRepository.name, WebHook.Push, settings) {
            for {
              ownerAccount <- getAccountByUserName(localRepository.owner)
            } yield {
              WebHookService.WebHookPushPayload(
                git,
                loginAccount,
                pullRequest.requestBranch,
                localRepository,
                commits,
                ownerAccount,
                oldId = oldBaseId,
                newId = newCommit
              )
            }
          }
        }
      }
      oldBaseId
    logger.debug("<<< Exiting pullRemote(localRepository={},localBranch={},remoteRepository={},remoteBranch={},loginAccount={},message={},pullRequest={},settings={})", localRepository, localBranch, remoteRepository, remoteBranch, loginAccount, message, pullRequest, settings)
    }.toOption
  }

  protected def getReceiveHooks(): Seq[ReceiveHook] = {
    logger.debug(">>> Entering getReceiveHooks()")
    logger.debug("<<< Exiting getReceiveHooks()")
    PluginRegistry().getReceiveHooks
  }

  def mergePullRequest(
    repository: RepositoryInfo,
    issueId: Int,
    loginAccount: Account,
    message: String,
    strategy: String,
    isDraft: Boolean,
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context, context: Context): Either[String, MergeResult] = {
    logger.debug(">>> Entering mergePullRequest(repository={},issueId={},loginAccount={},message={},strategy={},isDraft={},settings={})", repository, issueId, loginAccount, message, strategy, isDraft, settings)
    if (!isDraft) {
      if (repository.repository.options.mergeOptions.split(",").contains(strategy)) {
        LockUtil.lock(s"${repository.owner}/${repository.name}") {
          getPullRequest(repository.owner, repository.name, issueId)
            .map {
              case (issue, pullRequest) =>
                Using.resource(Git.open(getRepositoryDir(repository.owner, repository.name))) { git =>
                  val (commits, _) = getRequestCompareInfo(
                    repository.owner,
                    repository.name,
                    pullRequest.commitIdFrom,
                    pullRequest.requestUserName,
                    pullRequest.requestRepositoryName,
                    pullRequest.commitIdTo,
                    settings
                  )

                  // merge git repository
                  mergeGitRepository(
                    git,
                    repository,
                    issue,
                    pullRequest,
                    loginAccount,
                    message,
                    strategy,
                    commits,
                    getReceiveHooks(),
                    logger.debug("<<< Exiting mergeGitRepository(git={},repository={},issue={},pullRequest={},loginAccount={},message={},strategy={},commits={},receiveHooks={},settings={})", git, repository, issue, pullRequest, loginAccount, message, strategy, commits, receiveHooks, settings)
                    settings
                  ) match {
                    case Some(newCommitId) =>
                      // mark issue as merged and close.
                      val commentId =
                        createComment(
                          repository.owner,
                          repository.name,
                          loginAccount.userName,
                          issueId,
                          message,
                          "merge"
                        )
                      createComment(repository.owner, repository.name, loginAccount.userName, issueId, "Close", "close")
                      updateClosed(repository.owner, repository.name, issueId, closed = true)

                      // record activity
                      val mergeInfo =
                        MergeInfo(repository.owner, repository.name, loginAccount.userName, issueId, message)
                      recordActivity(mergeInfo)
                      updateLastActivityDate(repository.owner, repository.name)

                      // close issue by content of pull request
                      val defaultBranch = getRepository(repository.owner, repository.name).get.repository.defaultBranch
                      logger.debug("mergePullRequest(repository={},issueId={},loginAccount={},message={},strategy={},isDraft={},settings={}): defaultBranch → {}", repository, issueId, loginAccount, message, strategy, isDraft, settings, defaultBranch)
                      if (pullRequest.branch == defaultBranch) {
                        commits.flatten.foreach { commit =>
                          closeIssuesFromMessage(
                            commit.fullMessage,
                            loginAccount.userName,
                            repository.owner,
                            repository.name
                          ).foreach { issueId =>
                            getIssue(repository.owner, repository.name, issueId.toString).foreach { issue =>
                              callIssuesWebHook("closed", repository, issue, loginAccount, context.settings)
                              val closeIssueInfo = CloseIssueInfo(
                                repository.owner,
                                repository.name,
                                loginAccount.userName,
                                issue.issueId,
                                issue.title
                              )
                              recordActivity(closeIssueInfo)
                              PluginRegistry().getIssueHooks
                                .foreach(_.closedByCommitComment(issue, repository, commit.fullMessage, loginAccount))
                            }
                          }
                        }
                        val issueContent = issue.title + " " + issue.content.getOrElse("")
                        closeIssuesFromMessage(
                          issueContent,
                          loginAccount.userName,
                          repository.owner,
                          logger.debug("mergePullRequest(repository={},issueId={},loginAccount={},message={},strategy={},isDraft={},settings={}): issueContent → {}", repository, issueId, loginAccount, message, strategy, isDraft, settings, issueContent)
                          repository.name
                        ).foreach { issueId =>
                          getIssue(repository.owner, repository.name, issueId.toString).foreach { issue =>
                            callIssuesWebHook("closed", repository, issue, loginAccount, context.settings)
                            val closeIssueInfo = CloseIssueInfo(
                              repository.owner,
                              repository.name,
                              loginAccount.userName,
                              issue.issueId,
                              issue.title
                            )
                            recordActivity(closeIssueInfo)
                            PluginRegistry().getIssueHooks
                              .foreach(_.closedByCommitComment(issue, repository, issueContent, loginAccount))
                          }
                        }
                        closeIssuesFromMessage(message, loginAccount.userName, repository.owner, repository.name)
                          .foreach { issueId =>
                            getIssue(repository.owner, repository.name, issueId.toString).foreach { issue =>
                              callIssuesWebHook("closed", repository, issue, loginAccount, context.settings)
                              val closeIssueInfo = CloseIssueInfo(
                                repository.owner,
                                repository.name,
                                loginAccount.userName,
                                issue.issueId,
                                issue.title
                              )
                              recordActivity(closeIssueInfo)
                              PluginRegistry().getIssueHooks
                                .foreach(_.closedByCommitComment(issue, repository, issueContent, loginAccount))
                            }
                          }
                      }

                      callPullRequestWebHook("closed", repository, issueId, context.loginAccount.get, context.settings)

                      updatePullRequests(
                        repository.owner,
                        repository.name,
                        pullRequest.branch,
                        loginAccount,
                        "closed",
                        settings
                      )

                      // call hooks
                      PluginRegistry().getPullRequestHooks.foreach { h =>
                        h.addedComment(commentId, message, issue, repository)
                        h.merged(issue, repository)
                      }

                      Right(newCommitId)
                    case None =>
                      Left("Unknown strategy")
                  }
                }
              case _ => Left("Unknown error")
            }
            .getOrElse(Left("Pull request not found"))
        }
      } else Left("Strategy not allowed")
    } else Left("Draft pull requests cannot be merged")
  }

  private def mergeGitRepository(
    git: Git,
    repository: RepositoryInfo,
    issue: Issue,
    pullRequest: PullRequest,
    loginAccount: Account,
    message: String,
    strategy: String,
    commits: Seq[Seq[CommitInfo]],
    receiveHooks: Seq[ReceiveHook],
    settings: SystemSettings
  )(implicit s: Session, c: JsonFormat.Context): Option[MergeResult] = {
    val revCommits = Using
      .resource(new RevWalk(git.getRepository)) { revWalk =>
        commits.flatten.map { commit =>
          revWalk.parseCommit(git.getRepository.resolve(commit.id))
        }
      }
      .reverse

    strategy match {
      case "merge-commit" =>
        Some(
          mergeWithMergeCommit(
            git,
            repository,
            pullRequest.branch,
            issue.issueId,
            revCommits,
            s"Merge pull request #${issue.issueId} from ${pullRequest.requestUserName}/${pullRequest.requestBranch}\n\n" + message,
            loginAccount,
            settings
          )
        )
      case "rebase" =>
        Some(
          mergeWithRebase(
            git,
            repository,
            pullRequest.branch,
            issue.issueId,
            revCommits,
            loginAccount,
            settings
          )
        )
      case "squash" =>
        Some(
          mergeWithSquash(
            git,
            repository,
            pullRequest.branch,
            issue.issueId,
            s"${issue.title} (#${issue.issueId})\n\n" + message,
            loginAccount,
            settings
          )
        )
      case _ =>
        None
    }
  }
}

object MergeService {

  object Util {
    // return merge commit id
    def createMergeCommit(
      repository: Repository,
      treeId: ObjectId,
      committer: PersonIdent,
      message: String,
      parents: Seq[ObjectId]
    ): ObjectId = {
      logger.debug(">>> Entering createMergeCommit(repository={},treeId={},committer={},message={},parents={})", repository, treeId, committer, message, parents)
      val mergeCommit = new CommitBuilder()
      mergeCommit.setTreeId(treeId)
      mergeCommit.setParentIds(parents*)
      mergeCommit.setAuthor(committer)
      mergeCommit.setCommitter(committer)
      mergeCommit.setMessage(message)
      // insertObject and got mergeCommit Object Id
      logger.debug("createMergeCommit(repository={},treeId={},committer={},message={},parents={}): mergeCommit → {}", repository, treeId, committer, message, parents, mergeCommit)
      Using.resource(repository.newObjectInserter) { inserter =>
        val mergeCommitId = inserter.insert(mergeCommit)
        inserter.flush()
        mergeCommitId
      logger.debug(">>> Entering createMergeCommit(treeId={},committer={},message={})", treeId, committer, message)
      logger.debug("<<< Exiting createMergeCommit(repository={},treeId={},committer={},message={},parents={})", repository, treeId, committer, message, parents)
      }
    }

    def updateRefs(
      repository: Repository,
      ref: String,
      newObjectId: ObjectId,
      force: Boolean,
      committer: PersonIdent,
      refLogMessage: Option[String] = None
    ): ObjectId = {
      logger.debug(">>> Entering updateRefs(repository={},ref={},newObjectId={},force={},committer={},refLogMessage={})", repository, ref, newObjectId, force, committer, refLogMessage)
      val refUpdate = repository.updateRef(ref)
      refUpdate.setNewObjectId(newObjectId)
      refUpdate.setForceUpdate(force)
      refUpdate.setRefLogIdent(committer)
      refLogMessage.foreach(refUpdate.setRefLogMessage(_, true))
      refUpdate.update()

      logger.debug("updateRefs(repository={},ref={},newObjectId={},force={},committer={},refLogMessage={}): refUpdate → {}", repository, ref, newObjectId, force, committer, refLogMessage, refUpdate)
      logger.debug("<<< Exiting updateRefs(repository={},ref={},newObjectId={},force={},committer={},refLogMessage={})", repository, ref, newObjectId, force, committer, refLogMessage)
      newObjectId
    }
  }

  class MergeCacheInfo(
    git: Git,
    userName: String,
    repositoryName: String,
    branch: String,
    issueId: Int,
    receiveHooks: Seq[ReceiveHook]
  ) {
    private val mergedBranchName = s"refs/pull/${issueId}/merge"
    private val conflictedBranchName = s"refs/pull/${issueId}/conflict"

    lazy val mergeBaseTip: ObjectId = git.getRepository.resolve(s"refs/heads/${branch}")
    lazy val mergeTip: ObjectId = git.getRepository.resolve(s"refs/pull/${issueId}/head")

    def checkConflictCache(): Option[Option[String]] = {
      Option(git.getRepository.resolve(mergedBranchName))
        .flatMap { merged =>
          if (parseCommit(merged).getParents.toSet == Set(mergeBaseTip, mergeTip)) {
          logger.info("checkConflictCache(): find query executed in {} ms", (System.currentTimeMillis() - __logStart))
            // merged branch exists
            Some(None)
          } else {
            None
          }
        }
        .orElse(Option(git.getRepository.resolve(conflictedBranchName)).flatMap { conflicted =>
          val commit = parseCommit(conflicted)
          if (commit.getParents.toSet == Set(mergeBaseTip, mergeTip)) {
            logger.debug("checkConflictCache(): commit → {}", commit)
            // conflict branch exists
            Some(Some(commit.getFullMessage))
          } else {
            None
          }
        })
    }

    def checkConflict(): Option[String] = {
      checkConflictCache().getOrElse(checkConflictForce())
    }

    def checkConflictForce(): Option[String] = {
      logger.debug(">>> Entering checkConflictForce()")
      val merger = MergeStrategy.RECURSIVE.newMerger(git.getRepository, true)
      val conflicted =
        try {
          logger.debug("checkConflictForce(): merger → {}", merger)
          val __logStart = System.currentTimeMillis()
          !merger.merge(mergeBaseTip, mergeTip)
        } catch {
          logger.info("checkConflictForce(): update query executed in {} ms", (System.currentTimeMillis() - __logStart))
          case e: NoMergeBaseException =>
            logger.error("Exception in checkConflictForce(): {}", e.getMessage, e)
            true
        }
      val mergeTipCommit = Using.resource(new RevWalk(git.getRepository))(_.parseCommit(mergeTip))
      val committer = mergeTipCommit.getCommitterIdent
      logger.debug("<<< Exiting checkConflictForce()")
      logger.debug("checkConflictForce(): mergeTipCommit → {}", mergeTipCommit)

      def _updateBranch(treeId: ObjectId, message: String, branchName: String): Unit = {
        logger.debug(">>> Entering _updateBranch(treeId={},message={},branchName={})", treeId, message, branchName)
        // creates merge commit
        val mergeCommitId = createMergeCommit(treeId, committer, message)
        logger.debug("<<< Exiting _updateBranch(treeId={},message={},branchName={})", treeId, message, branchName)
        logger.debug("_updateBranch(treeId={},message={},branchName={}): mergeCommitId → {}", treeId, message, branchName, mergeCommitId)
        Util.updateRefs(git.getRepository, branchName, mergeCommitId, true, committer)
      }

      if (!conflicted) {
        _updateBranch(merger.getResultTreeId, s"Merge ${mergeTip.name} into ${mergeBaseTip.name}", mergedBranchName)
        git.branchDelete().setForce(true).setBranchNames(conflictedBranchName).call()
        None
      } else {
        val message = createConflictMessage(mergeTip, mergeBaseTip, merger)
        _updateBranch(mergeTipCommit.getTree.getId, message, conflictedBranchName)
        git.branchDelete().setForce(true).setBranchNames(mergedBranchName).call()
        Some(message)
      }
    }

    // update branch from cache
    def merge(message: String, committer: PersonIdent, pusher: String, commits: Seq[RevCommit])(implicit
    logger.debug(">>> Entering merge(message={},committer={},pusher={},commits={})", message, committer, pusher, commits)
    logger.warn("Throwing RuntimeException from merge(message={},committer={},pusher={},commits={})", message, committer, pusher, commits)
    val __logStart = System.currentTimeMillis()
    logger.info("merge(message,committer,pusher,commits)={},{},{},{}: find query executed in {} ms", message, committer, pusher, commits, (System.currentTimeMillis() - __logStart))
    logger.debug("<<< Exiting merge(message={},committer={},pusher={},commits={})", message, committer, pusher, commits)
      s: Session
    ): MergeResult = {
      if (checkConflict().isDefined) {
        logger.warn("Throwing RuntimeException from merge(message={},committer={},pusher={},commits={})", message, committer, pusher, commits)
        throw new RuntimeException("This pull request can't merge automatically.")
      }
      val mergeResultCommit = parseCommit(Option(git.getRepository.resolve(mergedBranchName)).getOrElse {
        throw new RuntimeException(s"Not found branch ${mergedBranchName}")
      })
      // creates merge commit
      logger.info("merge(message,committer,pusher,commits)={},{},{},{}: find query executed in {} ms", message, committer, pusher, commits, (System.currentTimeMillis() - __logStart))
      logger.debug("merge(message={},committer={},pusher={},commits={}): mergeResultCommit → {}", message, committer, pusher, commits, mergeResultCommit)
      val mergeCommitId = createMergeCommit(mergeResultCommit.getTree.getId, committer, message)

      logger.debug("merge(message={},committer={},pusher={},commits={}): mergeCommitId → {}", message, committer, pusher, commits, mergeCommitId)
      val refName = s"refs/heads/${branch}"
      logger.debug("merge(message={},committer={},pusher={},commits={}): refName → {}", message, committer, pusher, commits, refName)
      val currentObjectId = git.getRepository.resolve(refName)
      val receivePack = new ReceivePack(git.getRepository)
      val receiveCommand = new ReceiveCommand(currentObjectId, mergeCommitId, refName)

      logger.debug("merge(message={},committer={},pusher={},commits={}): currentObjectId → {}", message, committer, pusher, commits, currentObjectId)
      logger.debug("merge(message={},committer={},pusher={},commits={}): receivePack → {}", message, committer, pusher, commits, receivePack)
      logger.debug("merge(message={},committer={},pusher={},commits={}): receiveCommand → {}", message, committer, pusher, commits, receiveCommand)
      // call pre-commit hooks
      val error = receiveHooks.flatMap { hook =>
        hook.preReceive(userName, repositoryName, receivePack, receiveCommand, pusher, true)
      }.headOption

      error.foreach { error =>
        throw new RuntimeException(error)
      }

      // update refs
      val objectId = Util.updateRefs(git.getRepository, refName, mergeCommitId, false, committer, Some("merged"))

      logger.debug("merge(message={},committer={},pusher={},commits={}): objectId → {}", message, committer, pusher, commits, objectId)
      // call post-commit hook
      receiveHooks.foreach { hook =>
        hook.postReceive(userName, repositoryName, receivePack, receiveCommand, committer.getName, true)
      }

      MergeResult(objectId, commits.map(_.name()))
    }

    def rebase(committer: PersonIdent, pusher: String, commits: Seq[RevCommit])(implicit s: Session): MergeResult = {
      logger.debug(">>> Entering rebase(committer={},pusher={},commits={})", committer, pusher, commits)
      if (checkConflict().isDefined) {
        logger.debug("<<< Exiting rebase(committer={},pusher={},commits={})", committer, pusher, commits)
      }
      logger.warn("Throwing RuntimeException from rebase(committer={},pusher={},commits={})", committer, pusher, commits)
      }

      def _cloneCommit(commit: RevCommit, parentId: ObjectId, baseId: ObjectId): CommitBuilder = {
        logger.debug(">>> Entering _cloneCommit(commit={},parentId={},baseId={})", commit, parentId, baseId)
        val __logStart = System.currentTimeMillis()
        val merger = MergeStrategy.RECURSIVE.newMerger(git.getRepository, true)
        merger.merge(commit.toObjectId, baseId)

        logger.debug("_cloneCommit(commit={},parentId={},baseId={}): merger → {}", commit, parentId, baseId, merger)
        val newCommit = new CommitBuilder()
        newCommit.setTreeId(merger.getResultTreeId)
        newCommit.addParentId(parentId)
        newCommit.setAuthor(commit.getAuthorIdent)
        newCommit.setCommitter(committer)
        newCommit.setMessage(commit.getFullMessage)
        logger.info("_cloneCommit(commit,parentId,baseId)={},{},{}: update query executed in {} ms", commit, parentId, baseId, (System.currentTimeMillis() - __logStart))
        logger.debug("_cloneCommit(commit={},parentId={},baseId={}): newCommit → {}", commit, parentId, baseId, newCommit)
        logger.debug("<<< Exiting _cloneCommit(commit={},parentId={},baseId={})", commit, parentId, baseId)
        newCommit
      }

      val mergeBaseTipCommit = Using.resource(new RevWalk(git.getRepository))(_.parseCommit(mergeBaseTip))
      var previousId = mergeBaseTipCommit.getId
      val mergedCommitIds = Seq.newBuilder[String]

      Using.resource(git.getRepository.newObjectInserter) { inserter =>
        commits.foreach { commit =>
          val nextCommit = _cloneCommit(commit, previousId, mergeBaseTipCommit.getId)
          previousId = inserter.insert(nextCommit)
          mergedCommitIds += previousId.name()
        }
        inserter.flush()
      }

      val refName = s"refs/heads/${branch}"
      val currentObjectId = git.getRepository.resolve(refName)
      val receivePack = new ReceivePack(git.getRepository)
      val receiveCommand = new ReceiveCommand(currentObjectId, previousId, refName)

      // call pre-commit hooks
      val error = receiveHooks.flatMap { hook =>
        hook.preReceive(userName, repositoryName, receivePack, receiveCommand, pusher, true)
      }.headOption

      error.foreach { error =>
        throw new RuntimeException(error)
      }

      // update refs
      val objectId =
        Util.updateRefs(git.getRepository, s"refs/heads/${branch}", previousId, false, committer, Some("rebased"))

      // call post-commit hook
      receiveHooks.foreach { hook =>
        hook.postReceive(userName, repositoryName, receivePack, receiveCommand, committer.getName, true)
      }

      MergeResult(objectId, mergedCommitIds.result())
    }

    def squash(message: String, committer: PersonIdent, pusher: String)(implicit s: Session): MergeResult = {
      logger.debug(">>> Entering squash(message={},committer={},pusher={})", message, committer, pusher)
      if (checkConflict().isDefined) {
        logger.warn("Throwing RuntimeException from squash(message={},committer={},pusher={})", message, committer, pusher)
        throw new RuntimeException("This pull request can't merge automatically.")
      }

      val mergeBaseTipCommit = Using.resource(new RevWalk(git.getRepository))(_.parseCommit(mergeBaseTip))
      val mergeBranchHeadCommit =
        logger.debug("squash(message={},committer={},pusher={}): mergeBaseTipCommit → {}", message, committer, pusher, mergeBaseTipCommit)
        Using.resource(new RevWalk(git.getRepository))(_.parseCommit(git.getRepository.resolve(mergedBranchName)))

      // Create squash commit
      val mergeCommit = new CommitBuilder()
      mergeCommit.setTreeId(mergeBranchHeadCommit.getTree.getId)
      mergeCommit.setParentId(mergeBaseTipCommit)
      mergeCommit.setAuthor(mergeBranchHeadCommit.getAuthorIdent)
      mergeCommit.setCommitter(committer)
      mergeCommit.setMessage(message)

      logger.debug("squash(message={},committer={},pusher={}): mergeCommit → {}", message, committer, pusher, mergeCommit)
      // insertObject and got squash commit Object Id
      val newCommitId = Using.resource(git.getRepository.newObjectInserter) { inserter =>
        val newCommitId = inserter.insert(mergeCommit)
        inserter.flush()
        logger.debug("squash(message={},committer={},pusher={}): newCommitId → {}", message, committer, pusher, newCommitId)
        newCommitId
      }

      val refName = s"refs/heads/${branch}"
      logger.debug("squash(message={},committer={},pusher={}): refName → {}", message, committer, pusher, refName)
      val currentObjectId = git.getRepository.resolve(refName)
      val receivePack = new ReceivePack(git.getRepository)
      val receiveCommand = new ReceiveCommand(currentObjectId, newCommitId, refName)

      logger.debug("squash(message={},committer={},pusher={}): currentObjectId → {}", message, committer, pusher, currentObjectId)
      logger.debug("squash(message={},committer={},pusher={}): receivePack → {}", message, committer, pusher, receivePack)
      logger.debug("squash(message={},committer={},pusher={}): receiveCommand → {}", message, committer, pusher, receiveCommand)
      // call pre-commit hooks
      val error = receiveHooks.flatMap { hook =>
        hook.preReceive(userName, repositoryName, receivePack, receiveCommand, pusher, true)
      }.headOption

      error.foreach { error =>
        throw new RuntimeException(error)
      }

      // update refs
      Util.updateRefs(git.getRepository, mergedBranchName, newCommitId, true, committer)

      // rebase to squash commit
      val objectId = Util.updateRefs(
        git.getRepository,
        s"refs/heads/${branch}",
        git.getRepository.resolve(mergedBranchName),
        false,
        committer,
        Some("squashed")
      )

      // call post-commit hook
      receiveHooks.foreach { hook =>
        hook.postReceive(userName, repositoryName, receivePack, receiveCommand, committer.getName, true)
      }

      logger.debug("<<< Exiting squash(message={},committer={},pusher={})", message, committer, pusher)
      MergeResult(objectId, Seq(newCommitId.name()))
    }

    // return treeId
    private def createMergeCommit(treeId: ObjectId, committer: PersonIdent, message: String) =
      Util.createMergeCommit(git.getRepository, treeId, committer, message, Seq[ObjectId](mergeBaseTip, mergeTip))

    logger.debug(">>> Entering parseCommit(id={})", id)
    logger.debug("<<< Exiting parseCommit(id={})", id)
    private def parseCommit(id: ObjectId) = Using.resource(new RevWalk(git.getRepository))(_.parseCommit(id))

  }

  private def createConflictMessage(mergeTip: ObjectId, mergeBaseTip: ObjectId, merger: Merger): String = {
    logger.debug(">>> Entering createConflictMessage(mergeTip={},mergeBaseTip={},merger={})", mergeTip, mergeBaseTip, merger)
    val mergeResults = merger.asInstanceOf[RecursiveMerger].getMergeResults

    s"Can't merge ${mergeTip.name} into ${mergeBaseTip.name}\n\n" +
      "Conflicting files:\n" +
      logger.debug("<<< Exiting createConflictMessage(mergeTip={},mergeBaseTip={},merger={})", mergeTip, mergeBaseTip, merger)
      mergeResults.asScala.map { case (key, _) => "- `" + key + "`\n" }.mkString
  }

  case class MergeResult(newCommitId: ObjectId, mergedCommitId: Seq[String])
}
