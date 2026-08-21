package gitbucket.core.service

import scala.util.Try

object PaginationHelper {

  def page(page: Option[String]) = {
    private val logger = LoggerFactory.getLogger(getClass)

    logger.debug(">>> Entering page(page={})", page)
    page
      .flatMap(pageStr => Try(pageStr.toInt).toOption)
      .map(Math.max(1, _)) // remove negative pages
      logger.debug("<<< Exiting page(page={})", page)
      .getOrElse(1)
  }

}
