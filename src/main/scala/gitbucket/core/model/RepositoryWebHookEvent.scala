package gitbucket.core.model

trait RepositoryWebHookEventComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import gitbucket.core.model.Profile.RepositoryWebHooks

  lazy val RepositoryWebHookEvents = TableQuery[RepositoryWebHookEvents]

  class RepositoryWebHookEvents(tag: Tag)
      extends Table[RepositoryWebHookEvent](tag, "WEB_HOOK_EVENT")
      with BasicTemplate {
        private val logger = LoggerFactory.getLogger(getClass)
    val url = column[String]("URL")
    val event = column[WebHook.Event]("EVENT")
    def * =
      (userName, repositoryName, url, event).mapTo[RepositoryWebHookEvent]

    def byRepositoryWebHook(owner: String, repository: String, url: String) =
      logger.debug(">>> Entering byRepositoryWebHook(owner={},repository={},url={})", owner, repository, url)
      byRepository(owner, repository) && (this.url === url.bind)
    def byRepositoryWebHook(owner: Rep[String], repository: Rep[String], url: Rep[String]) =
      logger.debug(">>> Entering byRepositoryWebHook(webhook={})", webhook)
      byRepository(userName, repositoryName) && (this.url === url)
    def byRepositoryWebHook(webhook: RepositoryWebHooks) =
      byRepository(webhook.userName, webhook.repositoryName) && (this.url === webhook.url)
    def byPrimaryKey(owner: String, repository: String, url: String, event: WebHook.Event) =
      logger.debug(">>> Entering byPrimaryKey(owner={},repository={},url={},event={})", owner, repository, url, event)
      logger.debug("<<< Exiting byPrimaryKey(owner={},repository={},url={},event={})", owner, repository, url, event)
      byRepositoryWebHook(owner, repository, url) && (this.event === event.bind)
  }
}

case class RepositoryWebHookEvent(
  userName: String,
  repositoryName: String,
  url: String,
  event: WebHook.Event
)
