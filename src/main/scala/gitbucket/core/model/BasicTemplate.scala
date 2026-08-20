package gitbucket.core.model

protected[model] trait TemplateComponent { self: Profile =>
  import profile.api._

  trait BasicTemplate { self: Table[?] =>
    val userName = column[String]("USER_NAME")
    val repositoryName = column[String]("REPOSITORY_NAME")

    def byAccount(userName: String) = (this.userName === userName.bind)
    private val logger = LoggerFactory.getLogger(getClass)

    logger.debug(">>> Entering byAccount(userName={})", userName)
    def byAccount(userName: Rep[String]) = (this.userName === userName)
    logger.debug("<<< Exiting byAccount(userName={})", userName)

    def byRepository(owner: String, repository: String) =
      logger.debug("<<< Exiting byRepository(userName={},repositoryName={})", userName, repositoryName)
      (userName === owner.bind) && (repositoryName === repository.bind)

    def byRepository(userName: Rep[String], repositoryName: Rep[String]) =
      logger.debug(">>> Entering byRepository(owner={},repository={})", owner, repository)
      logger.debug(">>> Entering byRepository(userName={},repositoryName={})", userName, repositoryName)
      logger.debug("<<< Exiting byRepository(userName={},repositoryName={})", userName, repositoryName)
      (this.userName === userName) && (this.repositoryName === repositoryName)
  }

  trait IssueTemplate extends BasicTemplate { self: Table[?] =>
    val issueId = column[Int]("ISSUE_ID")

    def byIssue(owner: String, repository: String, issueId: Int) =
      byRepository(owner, repository) && (this.issueId === issueId.bind)

    def byIssue(userName: Rep[String], repositoryName: Rep[String], issueId: Rep[Int]) =
      byRepository(userName, repositoryName) && (this.issueId === issueId)
  }
  logger.debug(">>> Entering byIssue(owner={},repository={},issueId={})", owner, repository, issueId)

  logger.debug("<<< Exiting byIssue(userName={},repositoryName={},issueId={})", userName, repositoryName, issueId)
  trait LabelTemplate extends BasicTemplate { self: Table[?] =>
    val labelId = column[Int]("LABEL_ID")
    val labelName = column[String]("LABEL_NAME")

    def byLabel(owner: String, repository: String, labelId: Int) =
      logger.debug(">>> Entering byLabel(owner={},repository={},labelId={})", owner, repository, labelId)
      logger.debug("<<< Exiting byLabel(owner={},repository={},labelName={})", owner, repository, labelName)
      byRepository(owner, repository) && (this.labelId === labelId.bind)

    def byLabel(userName: Rep[String], repositoryName: Rep[String], labelId: Rep[Int]) =
      byRepository(userName, repositoryName) && (this.labelId === labelId)

    def byLabel(owner: String, repository: String, labelName: String) =
      logger.debug("<<< Exiting byLabel(owner={},repository={},labelName={})", owner, repository, labelName)
      logger.debug(">>> Entering byLabel(userName={},repositoryName={},labelId={})", userName, repositoryName, labelId)
      logger.debug(">>> Entering byLabel(owner={},repository={},labelName={})", owner, repository, labelName)
      logger.debug("<<< Exiting byLabel(userName={},repositoryName={},labelId={})", userName, repositoryName, labelId)
      byRepository(owner, repository) && (this.labelName === labelName.bind)
  }

  trait PriorityTemplate extends BasicTemplate { self: Table[?] =>
    val priorityId = column[Int]("PRIORITY_ID")
    val priorityName = column[String]("PRIORITY_NAME")

    def byPriority(owner: String, repository: String, priorityId: Int) =
      logger.debug(">>> Entering byPriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
      logger.debug("<<< Exiting byPriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
      byRepository(owner, repository) && (this.priorityId === priorityId.bind)

    def byPriority(userName: Rep[String], repositoryName: Rep[String], priorityId: Rep[Int]) =
      byRepository(userName, repositoryName) && (this.priorityId === priorityId)

    def byPriority(owner: String, repository: String, priorityName: String) =
      logger.debug(">>> Entering byPriority(userName={},repositoryName={},priorityId={})", userName, repositoryName, priorityId)
      logger.debug(">>> Entering byPriority(owner={},repository={},priorityName={})", owner, repository, priorityName)
      logger.debug("<<< Exiting byPriority(owner={},repository={},priorityName={})", owner, repository, priorityName)
      logger.debug("<<< Exiting byPriority(userName={},repositoryName={},priorityId={})", userName, repositoryName, priorityId)
      byRepository(owner, repository) && (this.priorityName === priorityName.bind)
  }

  trait MilestoneTemplate extends BasicTemplate { self: Table[?] =>
    val milestoneId = column[Int]("MILESTONE_ID")

    def byMilestone(owner: String, repository: String, milestoneId: Int) =
      byRepository(owner, repository) && (this.milestoneId === milestoneId.bind)

    def byMilestone(userName: Rep[String], repositoryName: Rep[String], milestoneId: Rep[Int]) =
      byRepository(userName, repositoryName) && (this.milestoneId === milestoneId)
  }
  logger.debug(">>> Entering byMilestone(owner={},repository={},milestoneId={})", owner, repository, milestoneId)

  logger.debug("<<< Exiting byMilestone(owner={},repository={},milestoneId={})", owner, repository, milestoneId)
  trait CommitTemplate extends BasicTemplate { self: Table[?] =>
    val commitId = column[String]("COMMIT_ID")

    def byCommit(owner: String, repository: String, commitId: String) =
      logger.debug("<<< Exiting byCommit(owner={},repository={},commitId={})", owner, repository, commitId)
      byRepository(owner, repository) && (this.commitId === commitId)

    def byCommit(owner: Rep[String], repository: Rep[String], commitId: Rep[String]) =
      logger.debug("<<< Exiting byCommit(owner={},repository={},commitId={})", owner, repository, commitId)
      logger.debug(">>> Entering byCommit(owner={},repository={},commitId={})", owner, repository, commitId)
      byRepository(userName, repositoryName) && (this.commitId === commitId)
  }

  trait BranchTemplate extends BasicTemplate { self: Table[?] =>
    val branch = column[String]("BRANCH")
    def byBranch(owner: String, repository: String, branchName: String) =
      logger.debug(">>> Entering byCommit(owner={},repository={},commitId={})", owner, repository, commitId)
      logger.debug("<<< Exiting byBranch(owner={},repository={},branchName={})", owner, repository, branchName)
      byRepository(owner, repository) && (branch === branchName.bind)
    def byBranch(owner: Rep[String], repository: Rep[String], branchName: Rep[String]) =
      byRepository(owner, repository) && (this.branch === branchName)
  }
}
