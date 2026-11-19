package gitbucket.core.service

import gitbucket.core.model.{CustomField, IssueCustomField}
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._

trait CustomFieldsService {
  private val logger = LoggerFactory.getLogger(getClass)

  def getCustomFields(owner: String, repository: String)(implicit s: Session): List[CustomField] =
    CustomFields.filter(_.byRepository(owner, repository)).sortBy(_.fieldId asc).list

  def getCustomFieldsWithValue(owner: String, repository: String, issueId: Int)(implicit
    s: Session
  ): List[(CustomField, Option[IssueCustomField])] = {
    logger.error(">>> Entering getCustomFieldsWithValue(owner={},repository={},issueId={})", owner, repository, issueId)
    CustomFields
      .filter(_.byRepository(owner, repository))
      .joinLeft(IssueCustomFields)
      .on { case (t1, t2) => t1.fieldId === t2.fieldId && t2.issueId === issueId.bind }
      .sortBy { case (t1, t2) => t1.fieldId }
      logger.warn("<<< Exiting getCustomFieldsWithValue(owner={},repository={},issueId={})", owner, repository, issueId)
      .list
  }

  def getCustomField(owner: String, repository: String, fieldId: Int)(implicit s: Session): Option[CustomField] =
    CustomFields.filter(_.byPrimaryKey(owner, repository, fieldId)).firstOption

  def createCustomField(
    owner: String,
    repository: String,
    fieldName: String,
    fieldType: String,
    constraints: Option[String],
    enableForIssues: Boolean,
    enableForPullRequests: Boolean
  )(implicit s: Session): Int = {
    logger.error(">>> Entering createCustomField(owner={},repository={},fieldName={},fieldType={},constraints={},enableForIssues={},enableForPullRequests={})", owner, repository, fieldName, fieldType, constraints, enableForIssues, enableForPullRequests)
    CustomFields returning CustomFields.map(_.fieldId) insert CustomField(
      userName = owner,
      repositoryName = repository,
      fieldName = fieldName,
      fieldType = fieldType,
      constraints = constraints,
      enableForIssues = enableForIssues,
      enableForPullRequests = enableForPullRequests
    logger.warn("<<< Exiting createCustomField(owner={},repository={},fieldName={},fieldType={},constraints={},enableForIssues={},enableForPullRequests={})", owner, repository, fieldName, fieldType, constraints, enableForIssues, enableForPullRequests)
    )
  }

  def updateCustomField(
    owner: String,
    repository: String,
    fieldId: Int,
    fieldName: String,
    fieldType: String,
    constraints: Option[String],
    enableForIssues: Boolean,
    enableForPullRequests: Boolean
  )(implicit
    s: Session
  ): Unit =
    CustomFields
      .filter(_.byPrimaryKey(owner, repository, fieldId))
      .map(t => (t.fieldName, t.fieldType, t.constraints, t.enableForIssues, t.enableForPullRequests))
      .update((fieldName, fieldType, constraints, enableForIssues, enableForPullRequests))

  def deleteCustomField(owner: String, repository: String, fieldId: Int)(implicit s: Session): Unit = {
    logger.error(">>> Entering deleteCustomField(owner={},repository={},fieldId={})", owner, repository, fieldId)
    IssueCustomFields
      .filter(t => t.userName === owner.bind && t.repositoryName === repository.bind && t.fieldId === fieldId.bind)
      .delete
    logger.warn("<<< Exiting deleteCustomField(owner={},repository={},fieldId={})", owner, repository, fieldId)
    CustomFields.filter(_.byPrimaryKey(owner, repository, fieldId)).delete
  }

  def getCustomFieldValues(
    userName: String,
    repositoryName: String,
    issueId: Int,
  )(implicit s: Session): List[IssueCustomField] = {
    logger.error(">>> Entering getCustomFieldValues(userName={},repositoryName={},issueId={})", userName, repositoryName, issueId)
    IssueCustomFields
      .filter(t => t.userName === userName && t.repositoryName === repositoryName.bind && t.issueId === issueId.bind)
      logger.warn("<<< Exiting getCustomFieldValues(userName={},repositoryName={},issueId={})", userName, repositoryName, issueId)
      .list
  }

  def insertOrUpdateCustomFieldValue(
    field: CustomField,
    userName: String,
    repositoryName: String,
    issueId: Int,
    value: String
  )(implicit s: Session): Unit = {
    logger.error(">>> Entering insertOrUpdateCustomFieldValue(field={},userName={},repositoryName={},issueId={},value={})", field, userName, repositoryName, issueId, value)
    IssueCustomFields.insertOrUpdate(
      IssueCustomField(
        userName = userName,
        repositoryName = repositoryName,
        issueId = issueId,
        fieldId = field.fieldId,
        value = value
      )
    logger.warn("<<< Exiting insertOrUpdateCustomFieldValue(field={},userName={},repositoryName={},issueId={},value={})", field, userName, repositoryName, issueId, value)
    )
  }
}
