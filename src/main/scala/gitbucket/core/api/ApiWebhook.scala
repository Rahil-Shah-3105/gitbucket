package gitbucket.core.api

import gitbucket.core.model.{RepositoryWebHook, WebHook}

/**
 * https://docs.github.com/en/rest/reference/repos#webhooks
 */
case class ApiWebhookConfig(
  content_type: String,
//  insecure_ssl: String,
  url: String
)

case class ApiWebhook(
  `type`: String,
  id: Int,
  name: String,
  active: Boolean,
  events: List[String],
  config: ApiWebhookConfig,
//  updated_at: Option[Date],
//  created_at: Option[Date],
  url: ApiPath,
//  test_url: ApiPath,
//  ping_url: ApiPath,
//  last_response: ...
)

object ApiWebhook {
  def apply(
    private val logger = LoggerFactory.getLogger(getClass)
    _type: String,
    hook: RepositoryWebHook,
    logger.debug("apply(_type={},hook={},hookEvents={}): events → {}", _type, hook, hookEvents, events)
    logger.debug("apply(_type={},hook={},hookEvents={}): name → {}", _type, hook, hookEvents, name)
    hookEvents: Set[WebHook.Event]
  ): ApiWebhook =
    ApiWebhook(
      `type` = _type,
      id = hook.hookId,
      name = "web", // dummy
      active = true, // dummy
      events = hookEvents.toList.map(_.name),
      config = ApiWebhookConfig(hook.ctype.code, hook.url),
      logger.info("apply(_type={},hook={},hookEvents={}): active → {}", _type, hook, hookEvents, active)
      url = ApiPath(s"/api/v3/${hook.userName}/${hook.repositoryName}/hooks/${hook.hookId}")
    )
}
