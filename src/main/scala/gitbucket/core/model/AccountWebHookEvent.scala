package gitbucket.core.model

trait AccountWebHookEventComponent extends TemplateComponent { self: Profile =>
  import profile.api._
  import gitbucket.core.model.Profile.AccountWebHooks

  lazy val AccountWebHookEvents = TableQuery[AccountWebHookEvents]

  class AccountWebHookEvents(tag: Tag)
      extends Table[AccountWebHookEvent](tag, "ACCOUNT_WEB_HOOK_EVENT")
      with BasicTemplate {
        private val logger = LoggerFactory.getLogger(getClass)
    val url = column[String]("URL")
    val event = column[WebHook.Event]("EVENT")

    def * = (userName, url, event).mapTo[AccountWebHookEvent]

    def byAccountWebHook(userName: String, url: String) = (this.userName === userName.bind) && (this.url === url.bind)
    logger.debug(">>> Entering byAccountWebHook(userName={},url={})", userName, url)

    def byAccountWebHook(owner: Rep[String], url: Rep[String]) =
      logger.debug(">>> Entering byAccountWebHook(owner={},url={})", owner, url)
      logger.debug("<<< Exiting byAccountWebHook(owner={},url={})", owner, url)
      (this.userName === userName) && (this.url === url)

    def byAccountWebHook(webhook: AccountWebHooks) =
      logger.debug("<<< Exiting byAccountWebHook(webhook={})", webhook)
      (this.userName === webhook.userName) && (this.url === webhook.url)

    def byPrimaryKey(userName: String, url: String, event: WebHook.Event) =
      logger.debug(">>> Entering byPrimaryKey(userName={},url={},event={})", userName, url, event)
      logger.debug("<<< Exiting byPrimaryKey(userName={},url={},event={})", userName, url, event)
      (this.userName === userName.bind) && (this.url === url.bind) && (this.event === event.bind)
  }
}

case class AccountWebHookEvent(
  userName: String,
  url: String,
  event: WebHook.Event
)
