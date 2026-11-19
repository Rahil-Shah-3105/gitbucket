package gitbucket.core.service

import scala.util.Try

object PaginationHelper {
  private val logger = LoggerFactory.getLogger(getClass)

  def page(page: Option[String]) = {
    logger.error(">>> Entering page(page={})", page)

    page
      .flatMap(pageStr => Try(pageStr.toInt).toOption)
      .map(Math.max(1, _)) // remove negative pages
      logger.warn("<<< Exiting page(page={})", page)
      .getOrElse(1)
  }

}
