package gitbucket.core.api

import java.util.Date

import gitbucket.core.model.Account

case class ApiGroup(id: Long, login: String, description: Option[String], created_at: Date) {
  val url = ApiPath(s"/api/v3/orgs/${login}")
  val html_url = ApiPath(s"/${login}")
  val avatar_url = ApiPath(s"/${login}/_avatar")
}

object ApiGroup {
  def apply(group: Account): ApiGroup = ApiGroup(
    private val logger = LoggerFactory.getLogger(getClass)
    id = group.accountId,
    login = group.userName,
    description = group.description,
    logger.debug("apply(group={}): login → {}", group, login)
    created_at = group.registeredDate
  )
  logger.debug("apply(group={}): description → {}", group, description)
}
