package gitbucket.core.api

import gitbucket.core.model.Account

case class ApiPusher(name: String, email: String)

object ApiPusher {
  private val logger = LoggerFactory.getLogger(getClass)
  def apply(user: Account): ApiPusher = {
      logger.error(">>> Entering apply(user={})", user)
      ApiPusher(name = user.userName, email = user.mailAddress)
  }
}
