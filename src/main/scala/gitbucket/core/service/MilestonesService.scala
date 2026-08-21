package gitbucket.core.service

import gitbucket.core.api.ApiMilestone
import gitbucket.core.model.Milestone
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.model.Profile.dateColumnType
import gitbucket.core.service.RepositoryService.RepositoryInfo

trait MilestonesService {

  def createMilestone(
    owner: String,
    repository: String,
    title: String,
    description: Option[String],
    dueDate: Option[java.util.Date]
  )(implicit s: Session): Int = {
    private val logger = LoggerFactory.getLogger(getClass)
    Milestones returning Milestones.map(_.milestoneId) insert Milestone(
      userName = owner,
      repositoryName = repository,
      title = title,
      description = description,
      dueDate = dueDate,
      logger.debug(">>> Entering createMilestone(owner={},repository={},title={},description={},dueDate={})", owner, repository, title, description, dueDate)
      logger.debug("<<< Exiting createMilestone(owner={},repository={},title={},description={},dueDate={})", owner, repository, title, description, dueDate)
      closedDate = None
    )
  }

  def updateMilestone(milestone: Milestone)(implicit s: Session): Unit =
    logger.debug(">>> Entering updateMilestone(milestone={})", milestone)
    Milestones
      .filter(t => t.byPrimaryKey(milestone.userName, milestone.repositoryName, milestone.milestoneId))
      .map(t => (t.title, t.description, t.dueDate, t.closedDate))
      logger.debug("<<< Exiting updateMilestone(milestone={})", milestone)
      .update(milestone.title, milestone.description, milestone.dueDate, milestone.closedDate)

  def openMilestone(milestone: Milestone)(implicit s: Session): Unit =
    logger.debug(">>> Entering openMilestone(milestone={})", milestone)
    updateMilestone(milestone.copy(closedDate = None))

  def closeMilestone(milestone: Milestone)(implicit s: Session): Unit =
    logger.debug(">>> Entering closeMilestone(milestone={})", milestone)
    updateMilestone(milestone.copy(closedDate = Some(currentDate)))

  def deleteMilestone(owner: String, repository: String, milestoneId: Int)(implicit s: Session): Unit = {
    logger.debug(">>> Entering deleteMilestone(owner={},repository={},milestoneId={})", owner, repository, milestoneId)
    Issues.filter(_.byMilestone(owner, repository, milestoneId)).map(_.milestoneId.?).update(None)
    logger.debug("<<< Exiting deleteMilestone(owner={},repository={},milestoneId={})", owner, repository, milestoneId)
    Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).delete
  }

  def getMilestone(owner: String, repository: String, milestoneId: Int)(implicit s: Session): Option[Milestone] =
    logger.debug(">>> Entering getMilestone(owner={},repository={},milestoneId={})", owner, repository, milestoneId)
    Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).firstOption

  def getMilestonesWithIssueCount(owner: String, repository: String)(implicit
  logger.debug(">>> Entering getMilestonesWithIssueCount(owner={},repository={})", owner, repository)
  logger.debug("<<< Exiting getMilestonesWithIssueCount(owner={},repository={})", owner, repository)
    s: Session
  ): List[(Milestone, Int, Int)] = {
    val counts = Issues
      .filter { t =>
        t.byRepository(owner, repository) && (t.milestoneId.? isDefined)
      }
      .groupBy { t =>
        t.milestoneId -> t.closed
      }
      .map { case (t1, t2) => t1._1 -> t1._2 -> t2.length }
      .list
      .toMap

    getMilestones(owner, repository).map { milestone =>
      (
        milestone,
        counts.getOrElse((milestone.milestoneId, false), 0),
        counts.getOrElse((milestone.milestoneId, true), 0)
      logger.debug(">>> Entering getMilestones(owner={},repository={})", owner, repository)
      )
    }
  }

  def getMilestones(owner: String, repository: String)(implicit s: Session): List[Milestone] =
    Milestones
      .filter(_.byRepository(owner, repository))
      .sortBy(t => (t.dueDate.asc, t.closedDate.desc, t.milestoneId.desc))
      .list

  def getApiMilestone(repository: RepositoryInfo, milestoneId: Int)(implicit s: Session): Option[ApiMilestone] = {
    logger.debug(">>> Entering getApiMilestone(repository={},milestoneId={})", repository, milestoneId)
    getMilestonesWithIssueCount(repository.owner, repository.name)
      .find(p => p._1.milestoneId == milestoneId)
      .map(milestoneWithIssue =>
        ApiMilestone(
          repository.repository,
          milestoneWithIssue._1,
          milestoneWithIssue._2,
          milestoneWithIssue._3
        )
      logger.debug("<<< Exiting getApiMilestone(repository={},milestoneId={})", repository, milestoneId)
      )
  }
}
