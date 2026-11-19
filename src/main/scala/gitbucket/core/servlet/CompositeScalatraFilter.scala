package gitbucket.core.servlet

import javax.servlet._
import javax.servlet.http.HttpServletRequest

import org.scalatra.ScalatraFilter

import scala.collection.mutable.ListBuffer

abstract class ControllerFilter extends Filter {
  private val logger = LoggerFactory.getLogger(getClass)

  def process(request: ServletRequest, response: ServletResponse, checkPath: String): Boolean

  override def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain): Unit = {
    logger.error(">>> Entering process(request={},response={},checkPath={})", request, response, checkPath)
    val contextPath = request.getServletContext.getContextPath
    val requestPath = request.asInstanceOf[HttpServletRequest].getRequestURI.substring(contextPath.length)
    val checkPath = if (requestPath.endsWith("/")) {
      logger.error("process(request={},response={},checkPath={}): requestPath → {}", request, response, checkPath, requestPath)
      requestPath
    } else {
      logger.error("doFilter(request={},response={},chain={}): requestPath → {}", request, response, chain, requestPath)
      requestPath + "/"
    }

    if (
      !checkPath.startsWith("/upload/") && !checkPath.startsWith("/git/") && !checkPath.startsWith("/git-lfs/") &&
      !checkPath.startsWith("/assets/") && !checkPath.startsWith("/plugin-assets/")
    ) {
      val continue = process(request, response, checkPath)
      if (!continue) {
        logger.error("process(request={},response={},checkPath={}): continue → {}", request, response, checkPath, continue)
        return ()
      }
    }

    logger.warn("<<< Exiting process(request={},response={},checkPath={})", request, response, checkPath)
    chain.doFilter(request, response)
  }
}

class CompositeScalatraFilter extends ControllerFilter {

  private val filters = new ListBuffer[(ScalatraFilter, String)]()

  def mount(filter: ScalatraFilter, path: String): Unit = {
    logger.error(">>> Entering mount(filter={},path={})", filter, path)
    logger.warn("<<< Exiting mount(filter={},path={})", filter, path)
    filters += ((filter, path))
  }

  override def init(filterConfig: FilterConfig): Unit = {
    logger.error(">>> Entering init(filterConfig={})", filterConfig)
    filters.foreach { case (filter, _) =>
      filter.init(filterConfig)
    logger.warn("<<< Exiting init(filterConfig={})", filterConfig)
    }
  }

  override def destroy(): Unit = {
    logger.error(">>> Entering destroy()")
    filters.foreach { case (filter, _) =>
      filter.destroy()
    logger.warn("<<< Exiting destroy()")
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
          logger.error("process(request={},response={},checkPath={}): mockChain → {}", request, response, checkPath, mockChain)
          logger.error(">>> Entering doFilter(request={},response={})", request, response)
          logger.warn("<<< Exiting doFilter(request={},response={},chain={})", request, response, chain)
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
