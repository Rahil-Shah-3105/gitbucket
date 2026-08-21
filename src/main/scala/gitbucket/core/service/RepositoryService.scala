package gitbucket.core.service

import gitbucket.core.controller.Context
import gitbucket.core.util._
import gitbucket.core.model.{CommitComments => _, Session => _, _}
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.Profile.dateColumnType
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.util.Directory.{getRepositoryDir, getRepositoryFilesDir, getTemporaryDir, getWikiRepositoryDir}
import gitbucket.core.util.JGitUtil.FileInfo
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.{Repository => _}

import scala.util.Using

trait RepositoryService {
  self: AccountService =>
  import RepositoryService._

  /**
   * Creates a new repository.
   *
   * @param repositoryName the repository name
   * @param userName the user name of the repository owner
   * @param description the repository description
   * @param isPrivate the repository type (private is true, otherwise false)
   * @param originRepositoryName specify for the forked repository. (default is None)
   * @param originUserName specify for the forked repository. (default is None)
   */
  def insertRepository(
    repositoryName: String,
    userName: String,
    description: Option[String],
    isPrivate: Boolean,
    defaultBranch: String,
    originRepositoryName: Option[String] = None,
    originUserName: Option[String] = None,
    parentRepositoryName: Option[String] = None,
    parentUserName: Option[String] = None
  )(implicit s: Session): Unit = {
    private val logger = LoggerFactory.getLogger(getClass)
    Repositories insert
    logger.debug(">>> Entering insertRepository(repositoryName={},userName={},description={},isPrivate={},defaultBranch={},originRepositoryName={},originUserName={},parentRepositoryName={},parentUserName={})", repositoryName, userName, description, isPrivate, defaultBranch, originRepositoryName, originUserName, parentRepositoryName, parentUserName)
    logger.debug("insertRepository(repositoryName={},userName={},description={},isPrivate={},defaultBranch={},originRepositoryName={},originUserName={},parentRepositoryName={},parentUserName={}): options → {}", repositoryName, userName, description, isPrivate, defaultBranch, originRepositoryName, originUserName, parentRepositoryName, parentUserName, options)
      Repository(
        userName = userName,
        repositoryName = repositoryName,
        isPrivate = isPrivate,
        description = description,
        defaultBranch = defaultBranch,
        registeredDate = currentDate,
        updatedDate = currentDate,
        lastActivityDate = currentDate,
        originUserName = originUserName,
        originRepositoryName = originRepositoryName,
        parentUserName = parentUserName,
        parentRepositoryName = parentRepositoryName,
        options = RepositoryOptions(
          issuesOption = "PUBLIC", // TODO DISABLE for the forked repository?
          externalIssuesUrl = None,
          wikiOption = "PUBLIC", // TODO DISABLE for the forked repository?
          externalWikiUrl = None,
          allowFork = true,
          mergeOptions = "merge-commit,squash,rebase",
          defaultMergeOption = "merge-commit",
          safeMode = true
        )
      logger.debug("insertRepository(repositoryName={},userName={},description={},isPrivate={},defaultBranch={},originRepositoryName={},originUserName={},parentRepositoryName={},parentUserName={}): registeredDate → {}", repositoryName, userName, description, isPrivate, defaultBranch, originRepositoryName, originUserName, parentRepositoryName, parentUserName, registeredDate)
      logger.debug("insertRepository(repositoryName={},userName={},description={},isPrivate={},defaultBranch={},originRepositoryName={},originUserName={},parentRepositoryName={},parentUserName={}): updatedDate → {}", repositoryName, userName, description, isPrivate, defaultBranch, originRepositoryName, originUserName, parentRepositoryName, parentUserName, updatedDate)
      logger.debug("insertRepository(repositoryName={},userName={},description={},isPrivate={},defaultBranch={},originRepositoryName={},originUserName={},parentRepositoryName={},parentUserName={}): lastActivityDate → {}", repositoryName, userName, description, isPrivate, defaultBranch, originRepositoryName, originUserName, parentRepositoryName, parentUserName, lastActivityDate)
      )

    IssueId insert (userName, repositoryName, 0)
  }

  def renameRepository(oldUserName: String, oldRepositoryName: String, newUserName: String, newRepositoryName: String)(
  logger.debug(">>> Entering renameRepository(oldUserName={},oldRepositoryName={},newUserName={},newRepositoryName={})", oldUserName, oldRepositoryName, newUserName, newRepositoryName)
  logger.debug("<<< Exiting renameRepository(oldUserName={},oldRepositoryName={},newUserName={},newRepositoryName={})", oldUserName, oldRepositoryName, newUserName, newRepositoryName)
    implicit s: Session
  ): Unit = {
    getAccountByUserName(newUserName).foreach { _ =>
      (Repositories filter { t =>
        t.byRepository(oldUserName, oldRepositoryName)
      } firstOption).foreach { repository =>
        LockUtil.lock(s"${repository.userName}/${repository.repositoryName}") {
          // Update pull request source-repo columns (no foreign key constraint)
          PullRequests
            .filter { t =>
              (t.requestUserName === oldUserName.bind) && (t.requestRepositoryName === oldRepositoryName.bind)
            }
            .map(t => t.requestUserName -> t.requestRepositoryName)
            .update(newUserName, newRepositoryName)

          // Rename will cascade via constraint
          Repositories
            .filter(_.byRepository(oldUserName, oldRepositoryName))
            .map(t => t.userName -> t.repositoryName)
            .update(newUserName, newRepositoryName)

          // TODO Drop transferred owner from collaborators?

          // Move git repository
          val repoDir = getRepositoryDir(oldUserName, oldRepositoryName)
          if (repoDir.isDirectory) {
            logger.debug("renameRepository(oldUserName={},oldRepositoryName={},newUserName={},newRepositoryName={}): repoDir → {}", oldUserName, oldRepositoryName, newUserName, newRepositoryName, repoDir)
            FileUtils.moveDirectory(repoDir, getRepositoryDir(newUserName, newRepositoryName))
          }
          // Move wiki repository
          val wikiDir = getWikiRepositoryDir(oldUserName, oldRepositoryName)
          if (wikiDir.isDirectory) {
            logger.debug("renameRepository(oldUserName={},oldRepositoryName={},newUserName={},newRepositoryName={}): wikiDir → {}", oldUserName, oldRepositoryName, newUserName, newRepositoryName, wikiDir)
            FileUtils.moveDirectory(wikiDir, getWikiRepositoryDir(newUserName, newRepositoryName))
          }
          // Move files directory
          val filesDir = getRepositoryFilesDir(oldUserName, oldRepositoryName)
          if (filesDir.isDirectory) {
            logger.debug("renameRepository(oldUserName={},oldRepositoryName={},newUserName={},newRepositoryName={}): filesDir → {}", oldUserName, oldRepositoryName, newUserName, newRepositoryName, filesDir)
            FileUtils.moveDirectory(filesDir, getRepositoryFilesDir(newUserName, newRepositoryName))
          }
          // Delete parent directory
          FileUtil.deleteDirectoryIfEmpty(getRepositoryFilesDir(oldUserName, oldRepositoryName))

          // Call hooks
          if (oldUserName == newUserName) {
            PluginRegistry().getRepositoryHooks.foreach(_.renamed(oldUserName, oldRepositoryName, newRepositoryName))
          } else {
            PluginRegistry().getRepositoryHooks.foreach(_.transferred(oldUserName, newUserName, newRepositoryName))
          }
        }
      }
    }
  }

  def deleteRepository(repository: Repository)(implicit s: Session): Unit = {
    logger.debug(">>> Entering deleteRepository(repository={})", repository)
    LockUtil.lock(s"${repository.userName}/${repository.repositoryName}") {
      logger.debug("<<< Exiting deleteRepository(repository={})", repository)
      deleteRepositoryOnModel(repository.userName, repository.repositoryName)

      FileUtil.deleteRecursively(getRepositoryDir(repository.userName, repository.repositoryName))

      FileUtil.deleteRecursively(getWikiRepositoryDir(repository.userName, repository.repositoryName))
      FileUtil.deleteRecursively(getTemporaryDir(repository.userName, repository.repositoryName))
      FileUtil.deleteRecursively(getRepositoryFilesDir(repository.userName, repository.repositoryName))

      // Call hooks
      PluginRegistry().getRepositoryHooks.foreach(_.deleted(repository.userName, repository.repositoryName))
    }
  }

  private def deleteRepositoryOnModel(userName: String, repositoryName: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering deleteRepositoryOnModel(userName={},repositoryName={})", userName, repositoryName)
//    Activities.filter(_.byRepository(userName, repositoryName)).delete
    Collaborators.filter(_.byRepository(userName, repositoryName)).delete
    CommitComments.filter(_.byRepository(userName, repositoryName)).delete
    IssueLabels.filter(_.byRepository(userName, repositoryName)).delete
    Labels.filter(_.byRepository(userName, repositoryName)).delete
    IssueComments.filter(_.byRepository(userName, repositoryName)).delete
    PullRequests.filter(_.byRepository(userName, repositoryName)).delete
    IssueAssignees.filter(_.byRepository(userName, repositoryName)).delete
    Issues.filter(_.byRepository(userName, repositoryName)).delete
    Priorities.filter(_.byRepository(userName, repositoryName)).delete
    IssueId.filter(_.byRepository(userName, repositoryName)).delete
    Milestones.filter(_.byRepository(userName, repositoryName)).delete
    RepositoryWebHooks.filter(_.byRepository(userName, repositoryName)).delete
    RepositoryWebHookEvents.filter(_.byRepository(userName, repositoryName)).delete
    DeployKeys.filter(_.byRepository(userName, repositoryName)).delete
    ReleaseAssets.filter(_.byRepository(userName, repositoryName)).delete
    ReleaseTags.filter(_.byRepository(userName, repositoryName)).delete
    logger.debug("<<< Exiting deleteRepositoryOnModel(userName={},repositoryName={})", userName, repositoryName)
    Repositories.filter(_.byRepository(userName, repositoryName)).delete
  }

  /**
   * Returns the repository names of the specified user.
   *
   * @param userName the user name of repository owner
   * @return the list of repository names
   */
  def getRepositoryNamesOfUser(userName: String)(implicit s: Session): List[String] =
    logger.debug(">>> Entering getRepositoryNamesOfUser(userName={})", userName)
    logger.debug("<<< Exiting getRepositoryNamesOfUser(userName={})", userName)
    Repositories filter (_.userName === userName.bind) map (_.repositoryName) list

  /**
   * Returns the specified repository information.
   *
   * @param userName the user name of the repository owner
   * @param repositoryName the repository name
   * @return the repository information
   */
  def getRepository(userName: String, repositoryName: String)(implicit s: Session): Option[RepositoryInfo] = {
    logger.debug(">>> Entering getRepository(userName={},repositoryName={})", userName, repositoryName)
    (Repositories
      .join(Accounts)
      .on(_.userName === _.userName)
      .filter { case (t1, t2) =>
        logger.debug("<<< Exiting getRepository(userName={},repositoryName={})", userName, repositoryName)
        t1.byRepository(userName, repositoryName) && t2.removed === false.bind
      } firstOption) map { case (repository, account) =>
      // for getting issue count and pull request count
      val issues = Issues
        .filter { t =>
          t.byRepository(repository.userName, repository.repositoryName) && (t.closed === false.bind)
        }
        .map(_.pullRequest)
        .list

      new RepositoryInfo(
        JGitUtil.getRepositoryInfo(repository.userName, repository.repositoryName),
        repository,
        issues.count(_ == false),
        issues.count(_ == true),
        getForkedCount(
          repository.originUserName.getOrElse(repository.userName),
        logger.debug("<<< Exiting getForkedCount(userName={},repositoryName={})", userName, repositoryName)
          repository.originRepositoryName.getOrElse(repository.repositoryName)
        ),
        getOpenMilestones(repository.userName, repository.repositoryName),
        getRepositoryManagers(repository.userName, repository.repositoryName)
      )
    }
  }

  def getRepositoryById(repositoryId: Long)(implicit s: Session): Option[RepositoryInfo] =
    logger.debug(">>> Entering getRepositoryById(repositoryId={})", repositoryId)
    Repositories
      .filter(_.repositoryId === repositoryId.bind)
      .firstOption
      logger.debug("<<< Exiting getRepositoryById(repositoryId={})", repositoryId)
      .flatMap(r => getRepository(r.userName, r.repositoryName))

  /**
   * Returns the repositories except private repository that user does not have access right.
   * Include public repository, private own repository and private but collaborator repository.
   *
   * @param userName the user name of collaborator
   * @return the repository information list
   */
  def getAllRepositories(userName: String)(implicit s: Session): List[(String, String)] = {
    logger.debug(">>> Entering getAllRepositories(userName={})", userName)
    Repositories
      .filter { t1 =>
        (t1.isPrivate === false.bind) ||
        (t1.userName === userName.bind) || (t1.userName in (GroupMembers
          .filter(_.userName === userName.bind)
          .map(_.groupName))) ||
        (Collaborators.filter { t2 =>
          t2.byRepository(t1.userName, t1.repositoryName) &&
          ((t2.collaboratorName === userName.bind) || (t2.collaboratorName in GroupMembers
            .filter(_.userName === userName.bind)
            .map(_.groupName)))
        } exists)
      }
      .sortBy(_.lastActivityDate desc)
      .map { t =>
        (t.userName, t.repositoryName)
      }
      logger.debug("<<< Exiting getAllRepositories(userName={})", userName)
      .list
  }

  /**
   * Returns the all public repositories.
   *
   * @return the repository information list
   */
  def getPublicRepositories(withoutPhysicalInfo: Boolean = false)(implicit s: Session): List[RepositoryInfo] = {
    logger.debug(">>> Entering getPublicRepositories(withoutPhysicalInfo={})", withoutPhysicalInfo)
    Repositories
      .filter { t1 =>
        t1.isPrivate === false.bind
      }
      .sortBy(_.lastActivityDate desc)
      .list
      logger.debug("<<< Exiting getPublicRepositories(withoutPhysicalInfo={})", withoutPhysicalInfo)
      .map(createRepositoryInfo(_, withoutPhysicalInfo))
  }

  /**
   * Returns the list of repositories which are owned by the specified user.
   * This list includes group repositories if the specified user is a member of the group.
   */
  def getUserRepositories(userName: String, withoutPhysicalInfo: Boolean = false)(implicit
    s: Session
  ): List[RepositoryInfo] = {
    logger.debug(">>> Entering getUserRepositories(userName={},withoutPhysicalInfo={})", userName, withoutPhysicalInfo)
    Repositories
      .filter { t1 =>
        (t1.userName === userName.bind) || (t1.userName in (GroupMembers
          .filter(_.userName === userName.bind)
          .map(_.groupName))) ||
        (Collaborators.filter { t2 =>
          t2.byRepository(t1.userName, t1.repositoryName) &&
          ((t2.collaboratorName === userName.bind) || (t2.collaboratorName in GroupMembers
            .filter(_.userName === userName.bind)
            .map(_.groupName)))
        } exists)
      }
      .sortBy(_.lastActivityDate desc)
      .list
      logger.debug("<<< Exiting getUserRepositories(userName={},withoutPhysicalInfo={})", userName, withoutPhysicalInfo)
      .map(createRepositoryInfo(_, withoutPhysicalInfo))
  }

  /**
   * Returns the list of visible repositories for the specified user.
   * If repositoryUserName is given then filters results by repository owner.
   * This function is for plugin compatibility.
   *
   * @param loginAccount the logged in account
   * @param repositoryUserName the repository owner (if None then returns all repositories which are visible for logged in user)
   * @param withoutPhysicalInfo if true then the result does not include physical repository information such as commit count,
   *                            branches and tags
   * @return the repository information which is sorted in descending order of lastActivityDate.
   */
  def getVisibleRepositories(
    loginAccount: Option[Account],
    repositoryUserName: Option[String] = None,
    withoutPhysicalInfo: Boolean = false
  )(implicit s: Session): List[RepositoryInfo] =
    getVisibleRepositories(loginAccount, repositoryUserName, withoutPhysicalInfo, false)

  /**
   * Returns the list of visible repositories for the specified user.
   * If repositoryUserName is given then filters results by repository owner.
   *
   * @param loginAccount the logged in account
   * @param repositoryUserName the repository owner (if None then returns all repositories which are visible for logged in user)
   * @param withoutPhysicalInfo if true then the result does not include physical repository information such as commit count,
   *                            branches and tags
   * @param limit if true then result will include only repositories owned by the login account. otherwise, result will be all visible repositories.
   * @return the repository information which is sorted in descending order of lastActivityDate.
   */
  def getVisibleRepositories(
    loginAccount: Option[Account],
    repositoryUserName: Option[String],
    withoutPhysicalInfo: Boolean,
    logger.debug(">>> Entering getVisibleRepositories(loginAccount={},repositoryUserName={},withoutPhysicalInfo={},limit={})", loginAccount, repositoryUserName, withoutPhysicalInfo, limit)
    logger.debug("<<< Exiting getVisibleRepositories(loginAccount={},repositoryUserName={},withoutPhysicalInfo={},limit={})", loginAccount, repositoryUserName, withoutPhysicalInfo, limit)
    logger.debug("<<< Exiting getVisibleRepositories(loginAccount={},repositoryUserName={},withoutPhysicalInfo={})", loginAccount, repositoryUserName, withoutPhysicalInfo)
    limit: Boolean
  )(implicit s: Session): List[RepositoryInfo] = {
    (loginAccount match {
      // for Administrators
      case Some(x) if (x.isAdmin && !limit) =>
        Repositories
          .join(Accounts)
          .on(_.userName === _.userName)
          .filter { case (t1, t2) => t2.removed === false.bind }
          .map { case (t1, t2) => t1 }
      // for Normal Users
      case Some(x) =>
        Repositories
          .join(Accounts)
          .on(_.userName === _.userName)
          .filter { case (t1, t2) =>
            (t2.removed === false.bind) && ((t1.isPrivate === false.bind && !limit.bind) || (t1.userName === x.userName) ||
              (t1.userName in GroupMembers.filter(_.userName === x.userName.bind).map(_.groupName)) ||
              (Collaborators.filter { t3 =>
                t3.byRepository(t1.userName, t1.repositoryName) &&
                ((t3.collaboratorName === x.userName.bind) ||
                  (t3.collaboratorName in GroupMembers.filter(_.userName === x.userName.bind).map(_.groupName)))
              } exists))
          }
          .map { case (t1, t2) => t1 }
      // for Guests
      case None =>
        Repositories
          .join(Accounts)
          .on(_.userName === _.userName)
          .filter { case (t1, t2) => t1.isPrivate === false.bind && t2.removed === false.bind }
          .map { case (t1, t2) => t1 }
    }).filter { t =>
      repositoryUserName.map { userName =>
        t.userName === userName.bind
      } getOrElse LiteralColumn(true)
    }.sortBy(_.lastActivityDate desc)
      .list
      .map(createRepositoryInfo(_, withoutPhysicalInfo))
  }

  private def createRepositoryInfo(repository: Repository, withoutPhysicalInfo: Boolean = false)(implicit
  logger.debug(">>> Entering createRepositoryInfo(repository={},withoutPhysicalInfo={})", repository, withoutPhysicalInfo)
  logger.debug("<<< Exiting createRepositoryInfo(repository={},withoutPhysicalInfo={})", repository, withoutPhysicalInfo)
    s: Session
  ): RepositoryInfo = {
    new RepositoryInfo(
      if (withoutPhysicalInfo) {
        new JGitUtil.RepositoryInfo(repository.userName, repository.repositoryName)
      } else {
        JGitUtil.getRepositoryInfo(repository.userName, repository.repositoryName)
      },
      repository,
      if (withoutPhysicalInfo) {
        -1
      } else {
        getForkedCount(
          repository.originUserName.getOrElse(repository.userName),
        logger.debug(">>> Entering getForkedCount(userName={},repositoryName={})", userName, repositoryName)
          repository.originRepositoryName.getOrElse(repository.repositoryName)
        )
      },
      if (withoutPhysicalInfo) {
        Nil
      } else {
        getRepositoryManagers(repository.userName, repository.repositoryName)
      }
    )
  }

  /**
   * TODO It seems to be able to improve performance. For example, RequestCache can be used for getAccountByUserName call.
   */
  private def getRepositoryManagers(userName: String, repositoryName: String)(implicit s: Session): Seq[String] = {
    logger.debug(">>> Entering getRepositoryManagers(userName={},repositoryName={})", userName, repositoryName)
    (if (getAccountByUserName(userName).exists(_.isGroupAccount)) {
       logger.debug("<<< Exiting getRepositoryManagers(userName={},repositoryName={})", userName, repositoryName)
       getGroupMembers(userName).collect { case x if (x.isManager) => x.userName }
     } else {
       Seq(userName)
     }) ++ getCollaboratorUserNames(userName, repositoryName, Seq(Role.ADMIN))
  }

  /**
   * Updates the last activity date of the repository.
   */
  def updateLastActivityDate(userName: String, repositoryName: String)(implicit s: Session): Unit = {
    logger.debug(">>> Entering updateLastActivityDate(userName={},repositoryName={})", userName, repositoryName)
    logger.debug("<<< Exiting updateLastActivityDate(userName={},repositoryName={})", userName, repositoryName)
    Repositories.filter(_.byRepository(userName, repositoryName)).map(_.lastActivityDate).update(currentDate)
  }

  /**
   * Save repository options.
   */
  def saveRepositoryOptions(
    userName: String,
    repositoryName: String,
    description: Option[String],
    isPrivate: Boolean,
    issuesOption: String,
    externalIssuesUrl: Option[String],
    wikiOption: String,
    externalWikiUrl: Option[String],
    allowFork: Boolean,
    mergeOptions: Seq[String],
    defaultMergeOption: String,
    logger.debug(">>> Entering saveRepositoryOptions(userName={},repositoryName={},description={},isPrivate={},issuesOption={},externalIssuesUrl={},wikiOption={},externalWikiUrl={},allowFork={},mergeOptions={},defaultMergeOption={},safeMode={})", userName, repositoryName, description, isPrivate, issuesOption, externalIssuesUrl, wikiOption, externalWikiUrl, allowFork, mergeOptions, defaultMergeOption, safeMode)
    logger.debug("<<< Exiting saveRepositoryOptions(userName={},repositoryName={},description={},isPrivate={},issuesOption={},externalIssuesUrl={},wikiOption={},externalWikiUrl={},allowFork={},mergeOptions={},defaultMergeOption={},safeMode={})", userName, repositoryName, description, isPrivate, issuesOption, externalIssuesUrl, wikiOption, externalWikiUrl, allowFork, mergeOptions, defaultMergeOption, safeMode)
    safeMode: Boolean
  )(implicit s: Session): Unit = {

    Repositories
      .filter(_.byRepository(userName, repositoryName))
      .map { r =>
        (
          r.description.?,
          r.isPrivate,
          r.issuesOption,
          r.externalIssuesUrl.?,
          r.wikiOption,
          r.externalWikiUrl.?,
          r.allowFork,
          r.mergeOptions,
          r.defaultMergeOption,
          r.safeMode,
          r.updatedDate
        )
      }
      .update(
        description,
        isPrivate,
        issuesOption,
        externalIssuesUrl,
        wikiOption,
        externalWikiUrl,
        allowFork,
        mergeOptions.mkString(","),
        defaultMergeOption,
        safeMode,
        currentDate
      )
  }

  def saveRepositoryDefaultBranch(userName: String, repositoryName: String, defaultBranch: String)(implicit
  logger.debug(">>> Entering saveRepositoryDefaultBranch(userName={},repositoryName={},defaultBranch={})", userName, repositoryName, defaultBranch)
  logger.debug("<<< Exiting saveRepositoryDefaultBranch(userName={},repositoryName={},defaultBranch={})", userName, repositoryName, defaultBranch)
    s: Session
  ): Unit =
    Repositories
      .filter(_.byRepository(userName, repositoryName))
      .map { r =>
        r.defaultBranch
      }
      .update(defaultBranch)

  /**
   * Add collaborator (user or group) to the repository.
   */
  def addCollaborator(userName: String, repositoryName: String, collaboratorName: String, role: String)(implicit
  logger.debug("<<< Exiting addCollaborator(userName={},repositoryName={},collaboratorName={},role={})", userName, repositoryName, collaboratorName, role)
    s: Session
  ): Unit =
    Collaborators insert Collaborator(userName, repositoryName, collaboratorName, role)

  /**
   * Remove specified collaborator from the repository.
   */
  def removeCollaborator(userName: String, repositoryName: String, collaboratorName: String)(implicit
  logger.debug("<<< Exiting removeCollaborator(userName={},repositoryName={},collaboratorName={})", userName, repositoryName, collaboratorName)
    s: Session
  ): Unit =
    Collaborators.filter(_.byPrimaryKey(userName, repositoryName, collaboratorName)).delete

  /**
   * Remove all collaborators from the repository.
   */
  def removeCollaborators(userName: String, repositoryName: String)(implicit s: Session): Unit =
    logger.debug(">>> Entering removeCollaborators(userName={},repositoryName={})", userName, repositoryName)
    logger.debug("<<< Exiting removeCollaborators(userName={},repositoryName={})", userName, repositoryName)
    Collaborators.filter(_.byRepository(userName, repositoryName)).delete

  /**
   * Returns the list of collaborators name (user name or group name) which is sorted with ascending order.
   */
  def getCollaborators(userName: String, repositoryName: String)(implicit s: Session): List[(Collaborator, Boolean)] =
    Collaborators
      .join(Accounts)
      .on(_.collaboratorName === _.userName)
      .filter { case (t1, t2) => t1.byRepository(userName, repositoryName) }
      .map { case (t1, t2) => (t1, t2.groupAccount) }
      .sortBy { case (t1, t2) => t1.collaboratorName }
      .list

  /**
   * Returns the list of all collaborator name and permission which is sorted with ascending order.
   * If a group is added as a collaborator, this method returns users who are belong to that group.
   */
  def getCollaboratorUserNames(userName: String, repositoryName: String, filter: Seq[Role] = Nil)(implicit
    s: Session
  ): List[String] = {
    logger.debug(">>> Entering getCollaboratorUserNames(userName={},repositoryName={},filter={})", userName, repositoryName, filter)
    val q1 = Collaborators
      .join(Accounts)
      .on { case (t1, t2) => (t1.collaboratorName === t2.userName) && (t2.groupAccount === false.bind) }
      .filter { case (t1, t2) => t1.byRepository(userName, repositoryName) }
      .map { case (t1, t2) => (t1.collaboratorName, t1.role) }

    val q2 = Collaborators
      .join(Accounts)
      .on { case (t1, t2) => (t1.collaboratorName === t2.userName) && (t2.groupAccount === true.bind) }
      .join(GroupMembers)
      .on { case ((t1, t2), t3) => t2.userName === t3.groupName }
      .filter { case ((t1, t2), t3) => t1.byRepository(userName, repositoryName) }
      .map { case ((t1, t2), t3) => (t3.userName, t1.role) }

    q1.union(q2)
      .list
      .filter { x =>
        filter.isEmpty || filter.exists(_.name == x._2)
      }
      logger.debug("<<< Exiting getCollaboratorUserNames(userName={},repositoryName={},filter={})", userName, repositoryName, filter)
      .map(_._1)
  }

  def hasOwnerRole(owner: String, repository: String, loginAccount: Option[Account])(implicit s: Session): Boolean = {
    logger.debug(">>> Entering hasOwnerRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
    loginAccount match {
      case Some(a) if a.isAdmin                                                                         =>
        logger.debug("<<< Exiting hasOwnerRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
        true
      case Some(a) if a.userName == owner                                                               => true
      case Some(a) if getGroupMembers(owner).exists(_.userName == a.userName)                           => true
      case Some(a) if getCollaboratorUserNames(owner, repository, Seq(Role.ADMIN)).contains(a.userName) => true
      case _                                                                                            => false
    }
  }

  def hasDeveloperRole(owner: String, repository: String, loginAccount: Option[Account])(implicit
  logger.debug(">>> Entering hasDeveloperRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
  logger.debug("<<< Exiting hasDeveloperRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
    s: Session
  ): Boolean = {
    loginAccount match {
      case Some(a) if a.isAdmin                                               => true
      case Some(a) if a.userName == owner                                     => true
      case Some(a) if getGroupMembers(owner).exists(_.userName == a.userName) => true
      case Some(a)
          if getCollaboratorUserNames(owner, repository, Seq(Role.ADMIN, Role.DEVELOPER)).contains(a.userName) =>
        true
      case _ => false
    }
  }

  def hasGuestRole(owner: String, repository: String, loginAccount: Option[Account])(implicit s: Session): Boolean = {
    logger.debug(">>> Entering hasGuestRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
    loginAccount match {
      case Some(a) if a.isAdmin                                               =>
        logger.debug("<<< Exiting hasGuestRole(owner={},repository={},loginAccount={})", owner, repository, loginAccount)
        true
      case Some(a) if a.userName == owner                                     => true
      case Some(a) if getGroupMembers(owner).exists(_.userName == a.userName) => true
      case Some(a)
          if getCollaboratorUserNames(owner, repository, Seq(Role.ADMIN, Role.DEVELOPER, Role.GUEST))
            .contains(a.userName) =>
        true
      case _ => false
    }
  }

  def isReadable(repository: Repository, loginAccount: Option[Account])(implicit s: Session): Boolean = {
    logger.debug(">>> Entering isReadable(repository={},loginAccount={})", repository, loginAccount)
    if (!repository.isPrivate) {
      logger.debug("<<< Exiting isReadable(repository={},loginAccount={})", repository, loginAccount)
      true
    } else {
      loginAccount match {
        case Some(x) if x.isAdmin                                                             => true
        case Some(x) if repository.userName == x.userName                                     => true
        case Some(x) if getGroupMembers(repository.userName).exists(_.userName == x.userName) => true
        case Some(x) if getCollaboratorUserNames(repository.userName, repository.repositoryName).contains(x.userName) =>
          true
        case _ => false
      }
    }
  }

  def isWritable(repository: Repository, loginAccount: Option[Account])(implicit s: Session): Boolean = {
    logger.debug(">>> Entering isWritable(repository={},loginAccount={})", repository, loginAccount)
    loginAccount match {
      case Some(x) if x.isAdmin                                                             =>
        logger.debug("<<< Exiting isWritable(repository={},loginAccount={})", repository, loginAccount)
        true
      case Some(x) if repository.userName == x.userName                                     => true
      case Some(x) if getGroupMembers(repository.userName).exists(_.userName == x.userName) => true
      case Some(x)
          if getCollaboratorUserNames(repository.userName, repository.repositoryName, Seq(Role.ADMIN, Role.DEVELOPER))
            .contains(x.userName) =>
        true
      case _ => false
    }
  }

  private def getForkedCount(userName: String, repositoryName: String)(implicit s: Session): Int =
    Query(Repositories.filter { t =>
      logger.debug(">>> Entering getForkedCount(userName={},repositoryName={})", userName, repositoryName)
      (t.originUserName === userName.bind) && (t.originRepositoryName === repositoryName.bind)
    }.length).first

  private def getOpenMilestones(userName: String, repositoryName: String)(implicit s: Session): Int =
    Query(
      Milestones
    logger.debug(">>> Entering getOpenMilestones(userName={},repositoryName={})", userName, repositoryName)
        .filter(_.byRepository(userName, repositoryName))
        .filter(_.closedDate.isEmpty)
        .length
    logger.debug("<<< Exiting getOpenMilestones(userName={},repositoryName={})", userName, repositoryName)
    ).first

  def getForkedRepositories(
    userName: String,
    repositoryName: String,
    order: ForkedRepositoryOrder = ForkedRepositoryOrder.ByOwner
  )(implicit s: Session): List[Repository] = {
    logger.debug(">>> Entering getForkedRepositories(userName={},repositoryName={},order={})", userName, repositoryName, order)
    val query = Repositories.filter { t =>
      (t.originUserName === userName.bind) && (t.originRepositoryName === repositoryName.bind)
    }

    logger.debug("<<< Exiting getForkedRepositories(userName={},repositoryName={},order={})", userName, repositoryName, order)
    order match {
      case ForkedRepositoryOrder.ByOwner => query.sortBy(_.userName asc).list
      case ForkedRepositoryOrder.Oldest  => query.sortBy(t => (t.registeredDate.asc, t.repositoryId.asc)).list
      case ForkedRepositoryOrder.Newest  => query.sortBy(t => (t.registeredDate.desc, t.repositoryId.desc)).list
    }
  }

  private val templateExtensions = Seq("md", "markdown")

  /**
   * Returns content of template set per repository.
   *
   * @param repository the repository information
   * @param fileBaseName the file basename without extension of template
   * @return The content of template if the repository has it, otherwise empty string.
   */
  def getContentTemplate(repository: RepositoryInfo, fileBaseName: String)(implicit s: Session): String = {
    logger.debug(">>> Entering getContentTemplate(repository={},fileBaseName={})", repository, fileBaseName)
    val withExtFilenames = templateExtensions.map(extension => s"${fileBaseName.toLowerCase()}.${extension}")

    logger.debug("<<< Exiting getContentTemplate(repository={},fileBaseName={})", repository, fileBaseName)
    def choiceTemplate(files: List[FileInfo]): Option[FileInfo] =
      files
        .find { f =>
          logger.debug(">>> Entering choiceTemplate(files={})", files)
          logger.debug("<<< Exiting choiceTemplate(files={})", files)
          f.name.toLowerCase() == fileBaseName
        }
        .orElse {
          files.find(f => withExtFilenames.contains(f.name.toLowerCase()))
        }

    // Get template file from project root. When didn't find, will lookup default folder.
    Using.resource(Git.open(Directory.getRepositoryDir(repository.owner, repository.name))) { git =>
      // maxFiles = 1 means not get commit info because the only objectId and filename are necessary here
      choiceTemplate(JGitUtil.getFileList(git, repository.repository.defaultBranch, ".", maxFiles = 1))
        .orElse {
          choiceTemplate(JGitUtil.getFileList(git, repository.repository.defaultBranch, ".gitbucket", maxFiles = 1))
        }
        .map { file =>
          JGitUtil.getContentFromId(git, file.id, true).collect {
            case bytes if FileUtil.isText(bytes) => StringUtil.convertFromByteArray(bytes)
          }
        }
        .flatten
    } getOrElse ""
  }
}

object RepositoryService {
  sealed trait ForkedRepositoryOrder
  object ForkedRepositoryOrder {
    case object ByOwner extends ForkedRepositoryOrder
    case object Oldest extends ForkedRepositoryOrder
    case object Newest extends ForkedRepositoryOrder
  }

  case class RepositoryInfo(
    owner: String,
    name: String,
    repository: Repository,
    issueCount: Int,
    pullCount: Int,
    forkedCount: Int,
    milestoneCount: Int,
    branchList: Seq[String],
    tags: Seq[JGitUtil.TagInfo],
    managers: Seq[String]
  ) {

    /**
     * Creates instance with issue count and pull request count.
     */
    def this(
      repo: JGitUtil.RepositoryInfo,
      model: Repository,
      issueCount: Int,
      pullCount: Int,
      forkedCount: Int,
      milestoneCount: Int,
      managers: Seq[String]
    ) =
      this(
        repo.owner,
        repo.name,
        model,
        issueCount,
        pullCount,
        forkedCount,
        milestoneCount,
        repo.branchList,
        repo.tags,
        logger.debug(">>> Entering this(repo={},model={},forkedCount={},managers={})", repo, model, forkedCount, managers)
        logger.debug("<<< Exiting this(repo={},model={},issueCount={},pullCount={},forkedCount={},milestoneCount={},managers={})", repo, model, issueCount, pullCount, forkedCount, milestoneCount, managers)
        managers
      )

    /**
     * Creates instance without issue, pull request, and milestone count.
     */
    def this(repo: JGitUtil.RepositoryInfo, model: Repository, forkedCount: Int, managers: Seq[String]) =
      this(repo.owner, repo.name, model, 0, 0, forkedCount, 0, repo.branchList, repo.tags, managers)

    def httpUrl(implicit context: Context): String = RepositoryService.httpUrl(owner, name)
    def sshUrl(implicit context: Context): Option[String] = RepositoryService.sshUrl(owner, name)

    def splitPath(path: String): (String, String) = {
      logger.debug(">>> Entering splitPath(path={})", path)
      val names = (branchList ++ tags.map(_.name)).sortBy(_.length).reverse

      logger.debug("splitPath(path={}): names → {}", path, names)
      val id = names.collectFirst {
        case name if (path == name || path.startsWith(name + "/")) => name
      } getOrElse {
        logger.debug("splitPath(path={}): id → {}", path, id)
        path.split("/")(0)
      }

      logger.debug("<<< Exiting splitPath(path={})", path)
      (id, path.substring(id.length).stripPrefix("/"))
    }
  }

  def httpUrl(owner: String, name: String)(implicit context: Context): String =
    logger.debug(">>> Entering httpUrl(owner={},name={})", owner, name)
    logger.debug("<<< Exiting httpUrl(owner={},name={})", owner, name)
    s"${context.baseUrl}/git/${owner}/${name}.git"

  def sshUrl(owner: String, name: String)(implicit context: Context): Option[String] =
    logger.debug(">>> Entering sshUrl(owner={},name={})", owner, name)
    context.settings.sshUrl(owner, name)

  def openRepoUrl(openUrl: String)(implicit context: Context): String =
    logger.debug(">>> Entering openRepoUrl(openUrl={})", openUrl)
    logger.debug("<<< Exiting openRepoUrl(openUrl={})", openUrl)
    s"github-${context.platform}://openRepo/${openUrl}"

  def readmeFiles: Seq[String] =
    PluginRegistry().renderableExtensions.map { extension =>
      s"readme.${extension}"
    } ++ Seq("readme.txt", "readme")
}
