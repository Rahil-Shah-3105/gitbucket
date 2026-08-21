package gitbucket.core.servlet

import javax.servlet._
import javax.servlet.http.HttpServletRequest

import org.scalatra.ScalatraFilter

import scala.collection.mutable.ListBuffer

abstract class ControllerFilter extends Filter {

  def process(request: ServletRequest, response: ServletResponse, checkPath: String): Boolean

  override def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain): Unit = {
    private val logger = LoggerFactory.getLogger(getClass)
    val contextPath = request.getServletContext.getContextPath
    logger.debug(">>> Entering doFilter(request={},response={},chain={})", request, response, chain)
    val requestPath = request.asInstanceOf[HttpServletRequest].getRequestURI.substring(contextPath.length)
    val checkPath = if (requestPath.endsWith("/")) {
      logger.debug("doFilter(request={},response={},chain={}): requestPath → {}", request, response, chain, requestPath)
      logger.debug("doFilter(request={},response={},chain={}): checkPath → {}", request, response, chain, checkPath)
      requestPath
    } else {
      requestPath + "/"
    }

    if (
      !checkPath.startsWith("/upload/") && !checkPath.startsWith("/git/") && !checkPath.startsWith("/git-lfs/") &&
      !checkPath.startsWith("/assets/") && !checkPath.startsWith("/plugin-assets/")
    ) {
      val continue = process(request, response, checkPath)
      if (!continue) {
        logger.debug("doFilter(request={},response={},chain={}): continue → {}", request, response, chain, continue)
        logger.debug("<<< Exiting process(request={},response={},checkPath={})", request, response, checkPath)
        return ()
      }
    }

    logger.debug("<<< Exiting doFilter(request={},response={},chain={})", request, response, chain)
    chain.doFilter(request, response)
  }
}

class CompositeScalatraFilter extends ControllerFilter {

  private val filters = new ListBuffer[(ScalatraFilter, String)]()

  def mount(filter: ScalatraFilter, path: String): Unit = {
    logger.debug(">>> Entering mount(filter={},path={})", filter, path)
    logger.debug("<<< Exiting mount(filter={},path={})", filter, path)
    filters += ((filter, path))
  }

  override def init(filterConfig: FilterConfig): Unit = {
    logger.debug(">>> Entering init(filterConfig={})", filterConfig)
    filters.foreach { case (filter, _) =>
      filter.init(filterConfig)
    logger.debug("<<< Exiting init(filterConfig={})", filterConfig)
    }
  }

  override def destroy(): Unit = {
    logger.debug(">>> Entering destroy()")
    filters.foreach { case (filter, _) =>
      filter.destroy()
    logger.debug("<<< Exiting destroy()")
    }
  }

  override def process(request: ServletRequest, response: ServletResponse, checkPath: String): Boolean = {
    filters
      .filter { case (_, path) =>
        val start = path.replaceFirst("/\\*$", "/")
        checkPath.startsWith(start)
      }
      .foreach { case (filter, _) =>
        val mockChain = new MockFilterChain()
        filter.doFilter(request, response, mockChain)
        if (mockChain.continue == false) {
          logger.debug("doFilter(request={},response={}): continue → {}", request, response, continue)
          return false
        }
      }

    true
  }

}

class MockFilterChain extends FilterChain {
  var continue: Boolean = false

  override def doFilter(request: ServletRequest, response: ServletResponse): Unit = {
    continue = true
  }
}

//class FilterChainFilter(chain: FilterChain) extends Filter {
//  override def init(filterConfig: FilterConfig): Unit = ()
//  override def destroy(): Unit  = ()
//  override def doFilter(request: ServletRequest, response: ServletResponse, mockChain: FilterChain) = chain.doFilter(request, response)
//}
