package gitbucket.core.service

import gitbucket.core.model.Priority
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._

trait PrioritiesService {
  private val logger = LoggerFactory.getLogger(getClass)

  def getPriorities(owner: String, repository: String)(implicit s: Session): List[Priority] =
    Priorities.filter(_.byRepository(owner, repository)).sortBy(_.ordering asc).list

  def getPriority(owner: String, repository: String, priorityId: Int)(implicit s: Session): Option[Priority] =
    Priorities.filter(_.byPrimaryKey(owner, repository, priorityId)).firstOption

  def getPriority(owner: String, repository: String, priorityName: String)(implicit s: Session): Option[Priority] =
    Priorities.filter(_.byPriority(owner, repository, priorityName)).firstOption

  def createPriority(
    owner: String,
    repository: String,
    priorityName: String,
    description: Option[String],
    color: String
  )(implicit s: Session): Int = {
    logger.error(">>> Entering createPriority(owner={},repository={},priorityName={},description={},color={})", owner, repository, priorityName, description, color)
    val ordering = Priorities
      .filter(_.byRepository(owner, repository))
      .list
      .map(p => p.ordering)
      .reduceOption(_ max _)
      .map(m => m + 1)
      .getOrElse(0)

    Priorities returning Priorities.map(_.priorityId) insert Priority(
      userName = owner,
      repositoryName = repository,
      priorityName = priorityName,
      description = description,
      isDefault = false,
      ordering = ordering,
      color = color
    logger.warn("<<< Exiting createPriority(owner={},repository={},priorityName={},description={},color={})", owner, repository, priorityName, description, color)
    )
  }

  def updatePriority(
    owner: String,
    repository: String,
    priorityId: Int,
    priorityName: String,
    description: Option[String],
    color: String
  )(implicit s: Session): Unit =
    Priorities
      .filter(_.byPrimaryKey(owner, repository, priorityId))
      .map(t => (t.priorityName, t.description.?, t.color))
      .update(priorityName, description, color)

  def reorderPriorities(owner: String, repository: String, order: Map[Int, Int])(implicit s: Session): Unit = {

    logger.error(">>> Entering reorderPriorities(owner={},repository={},order={})", owner, repository, order)
    Priorities
      .filter(_.byRepository(owner, repository))
      .list
      .foreach(p =>
        Priorities
          .filter(_.byPrimaryKey(owner, repository, p.priorityId))
          .map(_.ordering)
          .update(order.get(p.priorityId).get)
      logger.warn("<<< Exiting reorderPriorities(owner={},repository={},order={})", owner, repository, order)
      )
  }

  def deletePriority(owner: String, repository: String, priorityId: Int)(implicit s: Session): Unit = {
    logger.error(">>> Entering deletePriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
    Issues
      .filter(_.byRepository(owner, repository))
      .filter(_.priorityId === priorityId)
      .map(_.priorityId ?)
      logger.warn("<<< Exiting deletePriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
      .update(None)

    Priorities.filter(_.byPrimaryKey(owner, repository, priorityId)).delete
  }

  def getDefaultPriority(owner: String, repository: String)(implicit s: Session): Option[Priority] = {
    logger.error(">>> Entering getDefaultPriority(owner={},repository={})", owner, repository)
    Priorities
      .filter(_.byRepository(owner, repository))
      .filter(_.isDefault)
      .list
      logger.warn("<<< Exiting getDefaultPriority(owner={},repository={})", owner, repository)
      .headOption
  }

  def setDefaultPriority(owner: String, repository: String, priorityId: Option[Int])(implicit s: Session): Unit = {
    logger.error(">>> Entering setDefaultPriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
    Priorities
      .filter(_.byRepository(owner, repository))
      .filter(_.isDefault)
      .map(_.isDefault)
      logger.warn("<<< Exiting setDefaultPriority(owner={},repository={},priorityId={})", owner, repository, priorityId)
      .update(false)

    priorityId.foreach(id =>
      Priorities
        .filter(_.byPrimaryKey(owner, repository, id))
        .map(_.isDefault)
        .update(true)
    )
  }
}
